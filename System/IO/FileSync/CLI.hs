{-# LANGUAGE OverloadedStrings #-}

module System.IO.FileSync.CLI where

import Prelude hiding (putStrLn)

import Control.Exception (IOException)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.State
import Data.Functor.Monadic
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Sequence as S
import qualified Data.Text as T
import System.FilePath
import System.IO.FileSync.Join
import System.IO.FileSync.JoinStrategies
import System.IO.FileSync.Sync
import System.IO.FileSync.Types
import System.REPL
import System.REPL.Prompt (putStrLn, prompt)
import System.REPL.Types (PathExistenceType(..))

type AppState = [FilePath]
type Cmd = Command (StateT AppState IO) T.Text ()

maim :: IO ()
maim = evalStateT repl []
   where
      repl = makeREPL commands cmdExit cmdUnknown prompt
                      [Handler unknownCommandHandler,
                       Handler otherIOErrorHandler]

      commands = [cmdSync, cmdList, cmdExcl, cmdHelp]

      cmdSync :: Cmd
      cmdSync = makeCommand3
         ":[s]ync"
         (defCommandTest [":s", ":sync"])
         "Synchronizes two directories."
         True
         (dirAsker "Enter source directory: ")
         (dirAsker "Enter target directory: ")
         joinStrategyAsker
         (\_ src' trg' (_,strat) -> do
             exclusions <- get
             let filtF = flip elem exclusions . normalise . _fileTreeDataPath . fst
                 src = LR src'
                 trg = RR trg'
             diff <- liftIO $ runEitherT (createDiffTree src trg)

             liftIO $ case diff of
                         (Left errs) -> putStrLn ("errors during diff tree creation" :: String)
                         (Right diff') -> syncForests strat src trg (filterForest filtF diff'))

      cmdList :: Cmd
      cmdList = makeCommand
         ":[l]ist"
         (flip elem [":l", ":list"] . T.strip)
         "List availble join strategies."
         (const . mapM_ (liftIO . putStrLn) . M.keys $ joinStrategies)

      cmdExcl :: Cmd
      cmdExcl = makeCommand1
         ":e[x]clude"
         (defCommandTest [":exclude", ":x"])
         "Sets a list of excluded directories (file format: one filepath per line)."
         False
         (existentFileAsker "Enter exlusions file: ")
         (\_ fp -> liftIO (readFile fp) >$> (map normalise . lines) >>= put)

      cmdHelp :: Cmd
      cmdHelp = makeCommand
         ":[h]elp"
         (defCommandTest [":h", ":help"])
         "Prints this help text."
         (const $ summarizeCommands commands)

      cmdExit :: Cmd
      cmdExit = makeCommandN
         ":[e]xit"
         (defCommandTest [":e", ":exit"])
         ("Exits the program.")
         False
         []
         (repeat lineAsker)
         (\_ _ -> return ())

      cmdUnknown :: Cmd
      cmdUnknown = makeCommandN
         "Unknown"
         (const True)
         "Unknown command."
         False
         []
         (repeat lineAsker)
         (\t _ -> liftIO $ putStrLn $ "Unknown command " ++ T.unpack t ++ ".")

      dirAsker :: MonadIO m => T.Text -> Asker' m FilePath
      dirAsker pr = writablefilepathAsker pr
                       (\fp -> genericTypeError $ errMsg fp)
                       (\(ex, fp) -> return $ if ex == IsDirectory then Right fp
                                              else Left $ genericPredicateError $ errMsg fp)
         where
            errMsg = (<> " is not a valid, writable directory.") . T.pack

      existentFileAsker :: MonadIO m => T.Text -> Asker' m FilePath
      existentFileAsker pr = writablefilepathAsker pr
                              (\fp -> genericTypeError $ errMsg fp)
                              (\(ex, fp) -> return $ if ex == IsFile then Right fp
                                                     else Left $ genericPredicateError $ errMsg fp)
         where
            errMsg = (<> " could not be read.") . T.pack

      joinStrategyAsker :: MonadIO m => Asker m T.Text (T.Text, JoinStrategy ())
      joinStrategyAsker = predAsker
         "Enter join strategy name: "
         (\t -> return $ case M.lookup (T.strip t) joinStrategies of
                            Nothing -> Left $ genericPredicateError "No strategy by that name."
                            Just s -> Right (t,s))

      unknownCommandHandler :: SomeCommandError -> StateT AppState IO ()
      unknownCommandHandler _ = liftIO $ putStrLn ("Malformed command.":: String)

      otherIOErrorHandler :: IOException -> StateT AppState IO ()
      otherIOErrorHandler _ = liftIO $ putStrLn ("IO exception!" :: String)

joinStrategies :: M.Map T.Text (JoinStrategy ())
joinStrategies = M.fromList
   [("simpleLeft", simpleLeftJoin),
    ("simpleRight", simpleRightJoin),
    ("simpleInner", simpleInnerJoin),
    ("simpleOuter", simpleOuterJoin),

    ("summaryLeft", runSummaryJoin summaryLeftJoin),
    ("summaryRight", runSummaryJoin summaryRightJoin),
    ("summaryInner", runSummaryJoin summaryInnerJoin),
    ("summaryOuter", runSummaryJoin summaryOuterJoin)
   ]

runSummaryJoin :: JoinStrategy (S.Seq FileAction) -> JoinStrategy ()
runSummaryJoin j lr rr fp t = do
   (b,s) <- j lr rr fp t
   performSummaryJoin lr rr s
   return (b, ())

{-# LANGUAGE OverloadedStrings #-}

-- |The command-line interface for the executable.
module System.IO.FileSync.CLI where

import Prelude hiding (putStrLn)

import Control.Exception (IOException)
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.State
import qualified Data.Conduit as Con
import qualified Data.Foldable as F
import qualified Data.HashSet as HS
import Data.Functor.Monadic
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import qualified Data.Set as St
import qualified Data.Text as T
import System.Directory
import System.FilePath
import System.IO.FileSync.Join
import System.IO.FileSync.JoinStrategies
import System.IO.FileSync.Rename
import System.IO.FileSync.Sync
import System.IO.FileSync.Types
import System.REPL
import System.REPL.Prompt (putStrLn)
import System.REPL.Types (PathExistenceType(..))

import Debug.Trace

-- |The CLI's app state.
data AppState = AppState {
   _appStateExclusions :: HS.Set FilePath,
   _appStateLastDirs :: Maybe (FilePath, FilePath),
   _appStateConflicts :: [FilePath]
   }

type Cmd = Command (StateT AppState IO) T.Text ()

-- |Main command-line interface, in REPL-form.
cli :: IO ()
cli = do
   putStrLn ("FileSync v0.2" :: T.Text)
   putStrLn ("Sync directory trees." :: T.Text)
   putStrLn ("Enter :h or :help for a list of commands." :: T.Text)
   putStrLn ("Enter :exit to exit the program." :: T.Text)
   putStrLn ("" :: T.Text)
   evalStateT repl (AppState (HS.empty) Nothing [])
   where
      repl = makeREPLSimple commands

      commands = [cmdSync,
                  cmdList,
                  {- cmdExcl,
                  cmdListExcl,-}
                  cmdRename,
                  cmdDir,
                  cmdCd,
                  cmdHelp]

      cmdSync :: Cmd
      cmdSync = makeCommand3
         ":[s]ync"
         (defCommandTest [":s", ":sync"])
         "Synchronizes two directories."
         True
         (dirAsker "Enter source directory: ")
         (dirAsker "Enter target directory: ")
         joinStrategyAsker
         (\_ src' trg' (_,strat) -> sync src' trg' strat)

      cmdReplicate :: Cmd
      cmdReplicate = makeCommandN
         ":[r]eplicate"
         (defCommandTest [":r", ":replicate"])
         "Replicates a master onto multiple slaves."
         True
         [dirAsker "Enter master directory: ",
          dirAsker "Enter slave directory: "]
         (map (\i -> dirAsker $ "Enter slave directory " <> T.pack (show i) <> ": ") [2..])
         (\_ (master:slaves) -> mapM_ (flip (sync master) summaryLeftJoin) slaves)

      sync src' trg' strat = do
         exclusions <- _appStateExclusions <$> get
         let src = LR src'
             trg = RR trg'
         diff <- liftIO $ runEitherT (createDiffTree src trg)

         case diff of
            (Left errs) -> do
               let errs' = F.toList $ fmap (fromFTD) errs
               liftIO $ putStrLn ("There were file/directory-conflicts." :: String)
               liftIO $ putStrLn ("The following entries were present as files in one" :: String)
               liftIO $ putStrLn ("place and as directories in another:" :: String)
               liftIO $ putStrLn ("" :: String)
               liftIO $ mapM_ putStrLn errs'
               liftIO $ putStrLn ("" :: String)
               liftIO $ putStrLn ("Use ':rename' to rename the conflicting files." :: String)
               modify (\s -> s{_appStateConflicts = errs', _appStateLastDirs = Just (src', trg')})
            (Right diff') -> do
               let actions = syncForests strat src trg diff'
               liftIO (actions
                       Con.=$= askSummaryJoin src trg
                       Con.=$= reportSummaryJoin
                       Con.$$ performSummaryJoin src trg)
               clearConflicts

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
         True
         (existentFileAsker "Enter exlusions file: ")
         (\_ fp -> do
            excl <- liftIO (readFile fp >$> lines >$> map normalise' >$> HS.fromList)
            modify (\s -> s{_appStateExclusions=excl})
            liftIO $ putStrLn ("Read the list of exclusions." :: String))

      cmdListExcl :: Cmd
      cmdListExcl = makeCommand
         ":listExclusions (:[xl])"
         (defCommandTest [":listExclusions", ":xl"])
         "Lists the current exclusions."
         (\_ -> do
            excl <- _appStateExclusions <$> get
            if HS.null excl
            then liftIO $ putStrLn ("There are no exclusions." :: String)
            else liftIO $ mapM_ putStrLn (HS.toList excl))

      cmdRename :: Cmd
      cmdRename = makeCommand2
         ":[r]ename"
         (defCommandTest [":rename", ":r"])
         "Renames conflicting files in two directories."
         True
         (dirAsker "Enter the first directory: ")
         (dirAsker "Enter the second directory: ")
         (\_ src trg -> do
             let dirs = St.fromList [src, trg]
             sameDirs <- fromMaybe False
                         . fmap (\(s,t) -> dirs == St.fromList [s,t])
                         . _appStateLastDirs
                         <$> get

             -- If the directories are the same as during the last join attempts,
             -- we just get the stored conflicts.
             -- Otherwise, we create a new diff tree.
             conflicts <-
                if sameDirs then _appStateConflicts <$> get
                else do diff <- liftIO $ runEitherT $ createDiffTree (LR src) (RR trg)
                        return $ either (F.toList . fmap fromFTD) (const []) diff

             renamings <- liftIO $ renameConflicts [LR src, LR trg] conflicts
             liftIO $ mapM_ (\(r, o, n) -> putStrLn $ (r </> o) ++ " renamed to " ++ n) renamings
             clearConflicts
             liftIO $ putStrLn ("Conflicts cleared." :: String)
         )

      cmdDir :: Cmd
      cmdDir = makeCommand
         ":dir"
         (defCommandTest [":dir"])
         "Prints the current directory."
         (const $ liftIO $ getCurrentDirectory >>= putStrLn)

      cmdCd :: Cmd
      cmdCd = makeCommand1
         ":cd"
         (defCommandTest [":cd"])
         "Sets the current directory."
         True
         (dirAsker "Enter directory: ")
         (\_ dir -> do
            liftIO $ setCurrentDirectory dir
            dir' <- liftIO $ getCurrentDirectory
            liftIO $ putStrLn ("Current directory set to: \n" ++ dir'))

      cmdHelp :: Cmd
      cmdHelp = makeCommand
         ":[h]elp"
         (defCommandTest [":h", ":help"])
         "Prints this help text."
         (const $ summarizeCommands commands)

      -- Askers
      -------------------------------------------------------------------------

      dirAsker :: MonadIO m => T.Text -> Asker' m FilePath
      dirAsker pr = writableFilepathAsker pr
                       (\fp -> genericTypeError $ errMsg fp)
                       (\(ex, fp) -> return $ if ex == IsDirectory then Right fp
                                              else Left $ genericPredicateError $ errMsg fp)
         where
            errMsg = (<> " is not a valid, writable directory.") . T.pack

      existentFileAsker :: MonadIO m => T.Text -> Asker' m FilePath
      existentFileAsker pr = writableFilepathAsker pr
                              (\fp -> genericTypeError $ errMsg fp)
                              (\(ex, fp) -> return $ if ex == IsFile then Right fp
                                                     else Left $ genericPredicateError $ errMsg fp)
         where
            errMsg = (<> " could not be read.") . T.pack

      joinStrategyAsker :: MonadIO m => Asker m T.Text (T.Text, JoinStrategy FileAction)
      joinStrategyAsker = predAsker
         "Enter join strategy name: "
         (\t -> return $ case M.lookup (T.strip t) joinStrategies of
                            Nothing -> Left $ genericPredicateError "No strategy by that name."
                            Just s -> Right (t,s))

      otherIOErrorHandler :: IOException -> StateT AppState IO ()
      otherIOErrorHandler _ = liftIO $ putStrLn ("IO exception!" :: String)

joinStrategies :: M.Map T.Text (JoinStrategy FileAction)
joinStrategies = M.fromList
   [("left", summaryLeftJoin),
    ("right", summaryRightJoin),
    ("inner", summaryInnerJoin),
    ("outer", summaryOuterJoin)
   ]

-- |Clears the conflicts and the last dirs fields of the app state.
clearConflicts :: StateT AppState IO ()
clearConflicts = modify (\s -> s{_appStateConflicts = [], _appStateLastDirs = Nothing})

normalise' :: FilePath -> FilePath
normalise' = map (replace '\\' '/') . normalise
   where
      replace x y z | x == z = y
                    | otherwise = z

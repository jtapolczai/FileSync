{-# LANGUAGE OverloadedStrings #-}

module System.IO.FileSync.CLI where

import Prelude hiding (putStrLn)

import Control.Exception (IOException)
import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.Map as M
import qualified Data.Text as T
import System.IO.FileSync.JoinStrategies
import System.IO.FileSync.Sync
import System.IO.FileSync.Types
import System.REPL
import System.REPL.Prompt

maim :: IO ()
maim = makeREPL commands
                      cmdExit
                      cmdUnknown
                      prompt
                      [Handler unknownCommandHandler,
                       Handler otherIOErrorHandler]
   where
      commands = [cmdSync, cmdList, cmdHelp]

      cmdSync = makeCommand3
         ":[s]ync"
         (flip elem [":s", ":sync"] . T.strip)
         "Synchronizes two directories."
         True
         dirAsker
         dirAsker
         joinStrategyAsker
         (\_ src trg strat -> undefined)

      cmdList = makeCommand
         ":[l]ist"
         (flip elem [":l", ":list"] . T.strip)
         "List availble join strategies."
         (const . mapM_ putStrLn . M.keys $ joinStrategies)

      cmdHelp = makeCommand
         ":[h]elp"
         (flip elem [":h", ":help"] . T.strip)
         "Prints this help text."
         (const $ summarizeCommands commands)

      cmdExit = makeCommandN
         ":[e]xit"
         (flip elem [":e", ":exit"] . T.strip)
         ("Exits the program.")
         False
         []
         (repeat verbatimAsker)
         (\_ _ -> return ())

      cmdUnknown = makeCommandN
         "Unknown"
         (const True)
         "Unknown command."
         False
         []
         (repeat verbatimAsker)
         (\t _ -> putStrLn $ "Unknown command " ++ T.unpack t ++ ".")

      verbatimAsker :: Applicative m => Asker' m Verbatim
      verbatimAsker = typeAsker "" (const "BUG: Couldn't parse argument.")

      dirAsker :: MonadIO m => T.Text -> Asker' m FilePath
      dirAsker pr = writableFilepathAsker pr ()

      joinStrategyAsker :: MonadIO m => Asker m T.Text (T.Text, JoinStrategy (IO ()))
      joinStrategyAsker = undefined

      unknownCommandHandler :: SomeCommandError -> IO ()
      unknownCommandHandler _ = putStrLn ("Malformed command :(" :: String)

      otherIOErrorHandler :: IOException -> IO ()
      otherIOErrorHandler _ = putStrLn ("IO exception! :(" :: String)


joinStrategies :: M.Map T.Text (JoinStrategy (IO ()))
joinStrategies = M.fromList
   [("simpleLeft", simpleLeftJoin),
    ("simpleRight", simpleRightJoin)]
    -- rest: todo




{-# LANGUAGE OverloadedStrings #-}

module System.IO.FileSync.CLI where

import Prelude hiding (putStrLn)

import Control.Exception (IOException)
import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.Map as M
import qualified Data.Text.Lazy as TL
import System.IO.FileSync.JoinStrategies
import System.IO.FileSync.Sync
import System.IO.FileSync.Types
import System.REPL
import System.REPL.Command

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
         (flip elem [":s", ":sync"] . TL.strip)
         "Synchronizes two directories."
         True
         dirAsker
         dirAsker
         joinStrategyAsker
         (\_ src trg strat -> undefined)

      cmdList = makeCommand
         ":[l]ist"
         (flip elem [":l", ":list"] . TL.strip)
         "List availble join strategies."
         (const . mapM_ putStrLn . M.keys $ joinStrategies)

      cmdHelp = makeCommand
         ":[h]elp"
         (flip elem [":h", ":help"] . TL.strip)
         "Prints this help text."
         (const $ summarizeCommands commands)

      cmdExit = makeCommandN
         ":[e]xit"
         (flip elem [":e", ":exit"] . TL.strip)
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
         (\t _ -> putStrLn $ "Unknown command " ++ TL.unpack t ++ ".")

      verbatimAsker :: Monad m => Asker m Verbatim
      verbatimAsker = typeAsker "" "BUG: Couldn't parse argument."

      dirAsker :: MonadIO m => Asker m FilePath
      dirAsker = undefined

      joinStrategyAsker :: MonadIO m => Asker m (TL.Text, JoinStrategy (IO ()))
      joinStrategyAsker = undefined

      unknownCommandHandler :: SomeCommandError -> IO ()
      unknownCommandHandler _ = putStrLn ("Malformed command :(" :: String)

      otherIOErrorHandler :: IOException -> IO ()
      otherIOErrorHandler _ = putStrLn ("IO exception! :(" :: String)


joinStrategies :: M.Map TL.Text (JoinStrategy (IO ()))
joinStrategies = M.fromList
   [("simpleLeft", simpleLeftJoin),
    ("simpleRight", simpleRightJoin)]
    -- rest: todo




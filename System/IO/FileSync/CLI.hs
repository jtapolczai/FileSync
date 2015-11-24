{-# LANGUAGE OverloadedStrings #-}

module System.IO.FileSync.CLI where

import qualified Data.Map as M
import qualified Data.Text.Lazy as TL
import System.IO.FileSync.JoinStrategies
import System.IO.FileSync.Sync
import System.REPL
import System.REPL.Command

main :: IO ()
main = 
   where
      commands = [cmdSync, cmdList, cmdHelp]

      repl = makeREPL commands
                      cmdExit
                      cmdUnknown
                      (prompt "> ")
                      (---handler---)

      cmdSync = makeCommand2
         ":[s]ync"
         (flip elem [":s", ":sync"] . TL.strip)
         "Synchronizes two directories."
         True
         dirAsker
         dirAsker
         joinStrategyAsker
         (\_ src trg strat -> )

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

      joinStrategyAsker :: MonadIO m => Asker m (TL.Text, JoinStrategy)
      joinStrategyAsker = undefined

joinStrategies :: M.Map TL.Text JoinStrategy
joinStrategies = M.fromList
   [("simpleLeft", simpleLeftJoin),
    ("simpleRight", simpleRightJoin)]
    -- rest: todo




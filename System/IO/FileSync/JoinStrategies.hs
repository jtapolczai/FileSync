{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.IO.FileSync.JoinStrategies where

import Control.Exception
import qualified Data.Sequence as S
import qualified Data.Tree as T
import System.Directory
import System.FilePath
import System.IO.Error
import System.REPL (ask', typeAsker)

import System.IO.FileSync.Types

-- Simple joins
-------------------------------------------------------------------------------

-- |Left join. Performs all copying/deletions immediately.
simpleLeftJoin :: JoinStrategy (IO ())
simpleLeftJoin left right path (T.Node (LeftOnly, fp) _) =
   return (False, applyInsertAction left right $ path </> fp)
simpleLeftJoin _ right path (T.Node (RightOnly, fp) _) =
   return (False, applyDeleteAction right $ path </> fp)
simpleLeftJoin _ _ _ _ = return (True, return ())

-- |Right join. Performs all copying/deletions immediately.
simpleRightJoin :: JoinStrategy (IO ())
simpleRightJoin left _ path (T.Node (LeftOnly, fp) _) =
   return (False, applyDeleteAction left $ path </> fp)
simpleRightJoin left right path (T.Node (RightOnly, fp) _) =
   return (False, applyInsertAction right left $ path </> fp)
simpleRightJoin _ _ _ _ = return (True, return ())

-- |Inner join. Performs all copying/deletions immediately.
simpleInnerJoin :: JoinStrategy (IO ())
simpleInnerJoin left _ path (T.Node (LeftOnly, fp) _) =
   return (False, applyDeleteAction left $ path </> fp)
simpleInnerJoin _ right path (T.Node (RightOnly, fp) _) =
   return (False, applyDeleteAction right $ path </> fp)
simpleInnerJoin _ _ _ _ = return (True, return ())

-- |Full outer join. Performs all copying/deletions immediately.
simpleOuterJoin :: JoinStrategy (IO ())
simpleOuterJoin left right path (T.Node (LeftOnly, fp) _) =
   return (False, applyInsertAction left right $ path </> fp)
simpleOuterJoin left right path (T.Node (RightOnly, fp) _) =
   return (False, applyInsertAction right left $ path </> fp)
simpleOuterJoin _ _ _ _ = return (True, return ())

-- Summary joins
-------------------------------------------------------------------------------

-- |Generic summary join. Creates a list of actions to be performed,
--  which can be run with 'performSummaryJoin'.
summaryJoin
   :: (FilePath -> FileAction) -- ^Action for "left only" parts.
   -> (FilePath -> FileAction) -- ^Action for "right only" parts.
   -> JoinStrategy (S.Seq FileAction)
summaryJoin lA _ _ _ path (T.Node (LeftOnly, fp) _) =
   return (False, S.singleton $ lA $ path </> fp)
summaryJoin _ rA _ _ path (T.Node (RightOnly, fp) _) =
   return (False, S.singleton $ rA $ path </> fp)
summaryJoin _ _ _ _ _ _ = return (True, S.empty)

-- |Left join. See 'summaryJoin'.
summaryLeftJoin :: JoinStrategy (S.Seq FileAction)
summaryLeftJoin = summaryJoin (Copy LeftSide) (Delete RightSide)

-- |Right join. See 'summaryJoin'.
summaryRightJoin :: JoinStrategy (S.Seq FileAction)
summaryRightJoin = summaryJoin (Delete LeftSide) (Copy RightSide)

-- |Inner join. See 'summaryJoin'.
summaryInnerJoin :: JoinStrategy (S.Seq FileAction)
summaryInnerJoin = summaryJoin (Delete LeftSide) (Delete RightSide)

-- |Full outer join. See 'summaryJoin'.
summaryOuterJoin :: JoinStrategy (S.Seq FileAction)
summaryOuterJoin = summaryJoin (Copy LeftSide) (Copy RightSide)

-- |Takes a list of actions and runs them after asking the user
--  for confirmation. Iff the user cancels, the function returns 'False'.
performSummaryJoin :: LeftRoot -> RightRoot -> S.Seq FileAction -> IO Bool
performSummaryJoin left right actions = do
   putStrLn "You are about to perform the following operations:"
   putStrLn $ "Left directory: " ++ getFilePath left
   putStrLn $ "Right directory: " ++ getFilePath right
   mapM (putStrLn . showFileAction) actions
   putStrLn "Are you SURE (y/n)?"
   (answer :: YesNo) <- ask' $ typeAsker "Are you SURE (y/n)?" "Enter y for \"yes\" and n for \"no\"."
   case answer of
      Yes -> mapM (performFileAction left right) actions >> return True
      No -> putStrLn "Doing nothing." >> return False

-- |Shows a 'FileAction' as a short line.
--  The path is printed, along with a code:
-- 
--  1. "Copy to right": "C -->"
--  1. "Copy to left": "C <--"
--  1. "Delete from left": "D L"
--  1. "Delete from right": "D R"
showFileAction :: FileAction -> String
showFileAction (Copy LeftSide fp) =    "C -->:" ++ fp
showFileAction (Copy RightSide fp) =   "C <--:" ++ fp
showFileAction (Delete LeftSide fp) =  "D L:  " ++ fp
showFileAction (Delete RightSide fp) = "D R:  " ++ fp

-- |Performs a 'FileAction' (copying or deleting a file/directory).
--  Uses 'applyDeleteAction' and 'applyInsertAction'.
performFileAction :: LeftRoot -> RightRoot -> FileAction -> IO ()
performFileAction left right (Copy LeftSide fp) = applyInsertAction left right fp
performFileAction left right (Copy RightSide fp) = applyInsertAction right left fp
performFileAction left _ (Delete LeftSide fp) = applyDeleteAction left fp
performFileAction _ right (Delete RightSide fp) = applyDeleteAction right fp

-- Utility functions
-------------------------------------------------------------------------------


-- |Deletes a file or directory. Does not handle exceptions.
--  This function tries to remove the target of the given path
--  as a directory. If that fails, it tries to remove it as a
--  file.
--
--  The path to remove is given by @S </> P@.
applyDeleteAction
   :: FileRoot src
   => src -- |Root S of the source.
   -> FilePath -- ^Path P in the tree, starting from the root.
   -> IO ()
applyDeleteAction src path =
   catchJust noDirFoundException
            (removeDirectoryRecursive sPath)
            (const $ removeFile sPath)
   where
      sPath = getFilePath src </> path

-- |Copies a file or directory. This funciton tries to
--  to copy the given path as a directory. If that fails,
--  it tries to copy it as a file.
--
--  The source is given by @S </> P@. The target is given
--  by @T </> P@.
applyInsertAction
   :: (FileRoot src, FileRoot trg)
   => src -- |Root S of the source.
   -> trg -- |Root T of the target.
   -> FilePath -- ^Path P in the tree, starting from the roots.
   -> IO ()
applyInsertAction src trg path = 
   catchJust noDirFoundException
             (copyDirectory sPath tPath)
             (const $ copyFile sPath tPath)
   where
      sPath = getFilePath src </> path
      tPath = getFilePath trg </> path

-- |Copies a directory recursively. Tries to copy permissions.
--  Does not handle exceptions.
copyDirectory :: FilePath -> FilePath -> IO ()
copyDirectory src trg = go ""
   where
      go path = getDirectoryContents (src </> path) >>= mapM_ (tryCopyRec path)

      tryCopyRec :: FilePath -> FilePath -> IO ()
      tryCopyRec _ "." = return ()
      tryCopyRec _ ".." = return ()
      tryCopyRec path cur = let
            sPath = src </> path </> cur
            tPath = trg </> path </> cur
         in
            catchJust noDirFoundException
                     (do createDirectory tPath
                         copyPermissions sPath tPath
                         go $ path </> cur)
                     (\_ -> do copyFile sPath tPath
                               copyPermissions sPath tPath)


-- |Returns a Just iff the exception is of type "DoesNotExist"/"NoSuchThing".
noDirFoundException :: IOError -> Maybe ()
noDirFoundException e =
   if isDoesNotExistErrorType (ioeGetErrorType e)
   then Just () else Nothing

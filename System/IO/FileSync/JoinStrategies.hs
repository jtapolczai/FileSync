{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module System.IO.FileSync.JoinStrategies (
   -- * Simple joins
   simpleLeftJoin,
   simpleRightJoin,
   simpleInnerJoin,
   simpleOuterJoin,
   -- * Summary joins
   summaryJoin,
   summaryLeftJoin,
   summaryRightJoin,
   summaryInnerJoin,
   summaryOuterJoin,
   -- ** Utility functions for summary joins
   performFileAction,
   performSummaryJoin,
   showFileAction,
   -- ** Handlers for summary joins.
   overwriteWithNewer,
   overwriteWithLarger,
   overwriteWithLeft,
   overwriteWithRight,
   ) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Conduit as Con
import Data.Maybe
import qualified Data.Sequence as S
import qualified Data.Text as T
import qualified Data.Tree as Tr
import System.FilePath
import System.IO.Error
import System.REPL

import System.IO.FileSync.Types
import System.IO.FileSync.IO

-- |Uncomment this line to mock writing IO functions.
-- import System.IO.Mock
-- |Uncomment this line to use real writing IO functions.
import System.Directory

pattern JNode fp side <- Tr.Node (FTD fp _, side) _

-- Simple joins
-------------------------------------------------------------------------------

-- |Left join. Performs all copying/deletions immediately.
simpleLeftJoin :: JoinStrategy ()
simpleLeftJoin left right path (JNode fp LeftOnly) =
   liftIO (applyInsertAction left right (path </> fp)) >> Con.yield (Left No)
simpleLeftJoin _ right path (JNode fp RightOnly) =
   liftIO (applyDeleteAction right (path </> fp)) >> Con.yield (Left No)
simpleLeftJoin _ _ _ _ = Con.yield (Left Yes)

-- |Right join. Performs all copying/deletions immediately.
simpleRightJoin :: JoinStrategy ()
simpleRightJoin left _ path (JNode fp LeftOnly) =
   liftIO (applyDeleteAction left (path </> fp)) >> Con.yield (Left No)
simpleRightJoin left right path (JNode fp RightOnly) =
   liftIO (applyInsertAction right left (path </> fp)) >> Con.yield (Left No)
simpleRightJoin _ _ _ _ = Con.yield (Left Yes)

-- |Inner join. Performs all copying/deletions immediately.
simpleInnerJoin :: JoinStrategy ()
simpleInnerJoin left _ path (JNode fp LeftOnly) =
   liftIO (applyDeleteAction left (path </> fp)) >> Con.yield (Left No)
simpleInnerJoin _ right path (JNode fp RightOnly) =
   liftIO (applyDeleteAction right (path </> fp)) >> Con.yield (Left No)
simpleInnerJoin _ _ _ _ = Con.yield (Left Yes)

-- |Full outer join. Performs all copying/deletions immediately.
simpleOuterJoin :: JoinStrategy ()
simpleOuterJoin left right path (JNode fp LeftOnly) =
   liftIO (applyInsertAction left right (path </> fp)) >> Con.yield (Left No)
simpleOuterJoin left right path (JNode fp RightOnly) =
   liftIO (applyInsertAction right left (path </> fp)) >> Con.yield (Left No)
simpleOuterJoin _ _ _ _ = Con.yield (Left Yes)

-- Summary joins
-------------------------------------------------------------------------------

-- |Generic summary join. Creates a list of actions to be performed,
--  which can be run with 'performSummaryJoin'.
--
--  The Continue-part of the return value will be True iff the action handler
--  returns Nothing.
summaryJoin
   :: DifferenceHandler -- ^Action for "left only" parts.
   -> DifferenceHandler -- ^Action for "right only" parts.
   -> DifferenceHandler -- ^Action for parts present in both trees.
   -> JoinStrategy FileAction
summaryJoin lA rA bA _ _ path (JNode fp diff) = do
   let handler = case diff of {LeftOnly -> lA; RightOnly -> rA; Both -> bA}
   act <- liftIO $ handler (path </> fp)
   case act of
      Nothing -> Con.yield (Left Yes)
      Just act' -> Con.yield (Right act') >> Con.yield (Left No)
summaryJoin _ _ _ _ _ _ _ = error "summaryJoin: pattern match failure. This should never happen."

-- |Left join. See 'summaryJoin'.
summaryLeftJoin :: JoinStrategy FileAction
summaryLeftJoin = summaryJoin (return . Just . Copy LeftSide)
                              (return . Just . Delete RightSide)
                              (const $ return Nothing)

-- |Right join. See 'summaryJoin'.
summaryRightJoin :: JoinStrategy FileAction
summaryRightJoin = summaryJoin (return . Just . Delete LeftSide)
                               (return . Just . Copy RightSide)
                               (const $ return Nothing)

-- |Inner join. See 'summaryJoin'.
summaryInnerJoin :: JoinStrategy FileAction
summaryInnerJoin = summaryJoin (return . Just . Delete LeftSide)
                               (return . Just . Delete RightSide)
                               (const $ return Nothing)

-- |Full outer join. See 'summaryJoin'.
summaryOuterJoin :: JoinStrategy FileAction
summaryOuterJoin = summaryJoin (return . Just . Copy LeftSide)
                               (return . Just . Copy RightSide)
                               (const $ return Nothing)

-- |Takes a list of actions and runs them after asking the user
--  for confirmation. Iff the user cancels, the function returns 'False'.
performSummaryJoin :: LeftRoot -> RightRoot -> S.Seq FileAction -> IO Bool
performSummaryJoin left right actions = do
   putStrLn "You are about to perform the following operations:"
   putStrLn $ "Left directory: " ++ getFilePath left
   putStrLn $ "Right directory: " ++ getFilePath right
   mapM (putStrLn . showFileAction) actions
   putStrLn "Are you SURE (y/n)?"
   (answer :: YesNo) <- ask' yesNoAsker
   case answer of
      Yes -> mapM (performFileAction left right) actions >> return True
      No -> putStrLn "Doing nothing." >> return False

yesNoAsker :: Monad m => Asker m T.Text YesNo
yesNoAsker = predAsker
   "Perform these actions (y/n)?"
   (\t -> return $ if elem (T.strip t) ["y","Y"] then Right Yes
                   else if elem (T.strip t) ["n","N"] then Right No
                   else Left $ genericPredicateError "Expected y/n.")


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

-- Handlers for summary joins
-------------------------------------------------------------------------------

-- |Checks the modification times of two files/directories
--  and returns a 'FileAction' to copy the newer over the older one.
--
--  Returns Nothing if any one of these holds:
--
--  * Getting the modification time of either entry throws a 'PermissionError', or
--  * both entries have the same modification time.
--
--  Does not handle exceptions besides 'PermissionError'.
overwriteWithNewer :: LeftRoot
                   -> RightRoot
                   -> DifferenceHandler
overwriteWithNewer (LR lr) (RR rr) fp = let
      lfp = lr </> fp
      rfp = rr </> fp
   in do
      areDirs <- (&&) <$> doesDirectoryExist lfp <*> doesDirectoryExist rfp
      if areDirs then return Nothing
      else do lT <- handleIOErrors [isPermissionError] $ getModificationTime lfp
              rT <- handleIOErrors [isPermissionError] $ getModificationTime rfp
              return $ if lT > rT then Just (Copy LeftSide fp)
                       else if lT < rT then Just (Copy RightSide fp)
                       else Nothing

-- |Checks the modification times of two files/directories
--  and returns a 'FileAction' to copy the newer over the older one.
--
--  Returns Nothing if:
--
--  * Either of the two entries is not a file
--  * Getting the file size of either entry throws a 'PermissionError'.
--  * Both entries have the same size.
--
--  Does not handle exceptions besides 'InappropriateType' and 'PermissionError'.
overwriteWithLarger 
   :: LeftRoot
   -> RightRoot
   -> DifferenceHandler
overwriteWithLarger (LR lr) (RR rr) fp = do 
   lT <- handleIOErrors [isPermissionError, isInappropriateTypeError] $ getFileSize (lr </> fp)
   rT <- handleIOErrors [isPermissionError, isInappropriateTypeError] $ getFileSize (rr </> fp)
   return $ if lT > rT then Just (Copy LeftSide fp)
            else if lT < rT then Just (Copy RightSide fp)
            else Nothing

-- |If the path refers to a file, this function overwrites the right file with the left one.
--  Does nothing if the path refers to a directory.
overwriteWithLeft :: LeftRoot -> DifferenceHandler
overwriteWithLeft (LR root) fp = do
   isFile <- doesFileExist (root </> fp)
   return $ if isFile then Just $ Copy LeftSide fp else Nothing

-- |If the path refers to a file, this function overwrites the left file with the right one.
--  Does nothing if the path refers to a directory.
overwriteWithRight :: RightRoot -> DifferenceHandler
overwriteWithRight (RR root) fp = do
   isFile <- doesFileExist (root </> fp)
   return $ if isFile then Just $ Copy RightSide fp else Nothing

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
   catchThese [isDoesNotExistError, isInappropriateTypeError]
              (removeDirectoryRecursive sPath)
              (removeFile sPath)
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
   catchThese [isDoesNotExistError, isInappropriateTypeError]
              (copyDirectory sPath tPath)
              (copyFile sPath tPath)
   where
      sPath = getFilePath src </> path
      tPath = getFilePath trg </> path

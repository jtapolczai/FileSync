{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module System.IO.FileSync.JoinStrategies where

import Control.Exception
import qualified Data.Tree as T
import System.Directory
import System.FilePath
import System.IO.Error

import System.IO.FileSync.Types

simpleLeftJoin :: JoinStrategy (IO ())
simpleLeftJoin left right path (T.Node (LeftOnly, fp) _) =
   return (False, applyInsertAction left right $ path </> fp)
simpleLeftJoin _ right path (T.Node (RightOnly, fp) _) =
   return (False, applyDeleteAction right $ path </> fp)
simpleLeftJoin _ _ _ _ = return (True, return ())

simpleRightJoin :: JoinStrategy (IO ())
simpleRightJoin left _ path (T.Node (LeftOnly, fp) _) =
   return (False, applyDeleteAction left $ path </> fp)
simpleRightJoin left right path (T.Node (RightOnly, fp) _) =
   return (False, applyInsertAction right left $ path </> fp)
simpleRightJoin _ _ _ _ = return (True, return ())

simpleInnerJoin :: JoinStrategy (IO ())
simpleInnerJoin left _ path (T.Node (LeftOnly, fp) _) =
   return (False, applyDeleteAction left $ path </> fp)
simpleInnerJoin _ right path (T.Node (RightOnly, fp) _) =
   return (False, applyDeleteAction right $ path </> fp)
simpleInnerJoin _ _ _ _ = return (True, return ())

simpleOuterJoin :: JoinStrategy (IO ())
simpleOuterJoin left right path (T.Node (LeftOnly, fp) _) =
   return (False, applyInsertAction left right $ path </> fp)
simpleOuterJoin left right path (T.Node (RightOnly, fp) _) =
   return (False, applyInsertAction right left $ path </> fp)
simpleOuterJoin _ _ _ _ = return (True, return ())

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

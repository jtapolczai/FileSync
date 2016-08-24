-- |IO functions not found in the standard libraries.
module System.IO.FileSync.IO where

import Control.Exception
import GHC.IO.Exception (IOErrorType(InappropriateType, InvalidArgument))
import System.Directory
import System.FilePath
import System.IO
import System.IO.Error

-- import Debug.Trace

-- |Gets the size of a file in bytes.
--
--  May fail with:
--
--  * 'isInappropriateTypeError'
--  * 'isPermissionError'
--  * 'isDoesNotExistError'
--  * 'isAlreadyInUseError'
getFileSize :: FilePath -> IO Integer
getFileSize fp = do
   h <- openFile fp ReadMode
   size <- hFileSize h
   hClose h
   return size

-- |Copies a directory (or a file) recursively. Tries to copy permissions.
--  Does not handle exceptions. The target directory will be created if it does not exist.
copyDirectory :: FilePath -> FilePath -> IO ()
copyDirectory src trg = copyRec ""
   where
      -- Tries to copy a directory. Fails (safely) if (sPath </> path) is not a directory.
      go :: FilePath -> IO ()
      go path = do
         let sPath = src </> path
             tPath = trg </> path
         -- traceM $ "[copyDirectory] getting directory contents of " ++ sPath
         -- if this succeeds, we have an existent directory and try to copy it
         contents <- filter (not . flip elem [".",".."]) <$> getDirectoryContents sPath
         -- traceM ("[directoryStructureMatches] success.")
         createDirectory tPath
         copyPermissions sPath tPath
         -- recursive call. Note: no distinction between files and subdirectories.
         -- that's in copyRec.
         mapM_ (copyRec . (path </>)) contents

      copyRec :: FilePath -> IO ()
      copyRec path = catchThese [isDoesNotExistError,
                                 isInappropriateTypeError,
                                 isInvalidArgumentError]
                                (go path)
                                (do --traceM ("[directoryStructureMatches] failed, copying file instead.")
                                    copyFile (src </> path) (trg </> path)
                                    --traceM ("[directoryStructureMatches] success at copyFile.")
                                 )

-- |Returns True iff an 'IOError' is of type 'InappropriateType'. GHC-specific.
isInappropriateTypeError :: IOError -> Bool
isInappropriateTypeError = t . ioeGetErrorType
   where
      t InappropriateType = True
      t _ = False

-- |Returns True iff an 'IOError' is of type 'InvalidArgument'. GHC-specific.
isInvalidArgumentError :: IOError -> Bool
isInvalidArgumentError = t . ioeGetErrorType
   where
      t InvalidArgument = True
      t _ = False

-- Error handling
-------------------------------------------------------------------------------

-- |Catches all errors that fulfil at least one of the given predicates.
--  See 'catchJust'.
catchThese :: [IOError -> Bool]
           -> IO a -- ^Action A to perform
           -> IO a -- ^Action to perform instead if an error was caught.
           -> IO a
catchThese handlers action failureAction = catchJust
   (\e -> if any ($ e) handlers then Just () else Nothing)
   action
   (const failureAction)

-- |Executes an IO action, swallowing any error that passes at least one of the
--  selectors. If any of the handled errors occurrs, this function returns Nothing.
handleIOErrors :: [IOError -> Bool] -> IO a -> IO (Maybe a)
handleIOErrors handlers action = catchThese
   handlers
   (Just <$> action)
   (return Nothing)

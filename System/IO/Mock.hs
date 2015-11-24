module System.IO.Mock (
   copyFile,
   removeFile,
   removeDirectory,
   removeDirectoryRecursive,
   copyPermissions,
   createDirectory,
   ) where

import System.IO (stderr, hPutStrLn)

putErr = hPutStrLn stderr

copyFile :: FilePath -> FilePath -> IO ()
copyFile src trg = putErr ("copyFile: " ++ src) >> putErr ("   -->    " ++ trg)

removeFile :: FilePath -> IO ()
removeFile src = putErr ("removeFile: " ++ src)

removeDirectory :: FilePath -> IO ()
removeDirectory src = putErr ("removeDirectory: " ++ src)

removeDirectoryRecursive :: FilePath -> IO ()
removeDirectoryRecursive src = putErr ("removeDirectoryRecursive: " ++ src)

copyPermissions :: FilePath -> FilePath -> IO ()
copyPermissions src trg = putErr ("copyPermissions: " ++ src) >> putErr ("   -->           " ++ trg)

createDirectory :: FilePath -> IO ()
createDirectory src = putErr ("createDirectory: " ++ src)

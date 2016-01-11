module System.IO.FileSync.Rename where

import Control.Monad
import qualified Data.Foldable as F
import qualified Data.Sequence as S
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import System.Directory
import System.FilePath

import System.IO.FileSync.Types

-- |Goes through a list of paths and checks for their
--  presence in each given file root.
--  Where the entry is a file, it gets renamed by
--  appending a GUID to it.
renameConflicts
   :: (FileRoot fr, F.Foldable f, F.Foldable g)
   => f fr -- ^Possible roots for the entries.
   -> g FilePath -- ^List of offending entries. Each entry will be checked in each root.
   -> IO (S.Seq (FilePath, FilePath, FilePath)) -- ^List of renamings, in the format @(root, old name, new name)@.
renameConflicts roots = foldM f S.empty
   where
      f acc fp = (acc S.><) <$> renameConflict roots fp

-- |Renames a file by appending a GUID to its name
--  (but before its extension).
--
--  The entry will be looked for in each of the given roots
--  and a new GUID will be appended in __all__ roots where the entry
--  is found in form of a file.
renameConflict
   :: (FileRoot fr, F.Foldable f)
   => f fr -- ^Possible roots for the entries.
   -> FilePath -- ^Name of the offending entry. Each entry will be checked in each root.
   -> IO (S.Seq (FilePath, FilePath, FilePath)) -- ^List of renamings, in the format @(root, old name, new name)@.
renameConflict roots fp = foldM f S.empty roots
   where
      f acc root = do
         isFile <- doesFileExist $ getFilePath root </> fp
         if isFile then do
            uuid <- toString <$> nextRandom
            let name = takeBaseName fp ++ "_" ++ uuid
            return $ acc S.|> (getFilePath root, replaceBaseName fp name, fp)
         else return acc

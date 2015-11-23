{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module System.IO.FileSync.Sync where

import Data.List
import qualified Data.Tree as T
import System.Directory
import System.FilePath

import System.IO.FileSync.Join
import System.IO.FileSync.Types

-- |Takes a root directory and creates a tree representing its structure.
createFileTree
   :: FileRoot src
   => src
   -> IO (T.Tree FilePath)
createFileTree = go "" . getFilePath
   where
      go root this =  do
         let isFile x = doesFileExist (root </> this </> x) >>= return . (,x)
         contents <- getDirectoryContents (root </> this)
         (files, dirs) <- partition fst <$> mapM isFile contents
         let files' = map (\(_,x) -> T.Node x []) files
             dirs' = filter (not . flip elem [".",".."]) . map snd $ dirs
         children <- mapM (go $ root </> this) dirs'
         return $ T.Node this $ files' ++ children

-- |Creates a difference tree between two directories.
createDiffTree
   :: LeftRoot
   -> RightRoot
   -> IO (T.Tree (TreeDiff, FilePath))
createDiffTree src trg = (\s t -> head $ genericJoin [s] [t]) <$> createFileTree src <*> createFileTree trg

-- |Takes two root directories and synchronizes the tree that starts in them
--  using a given strategy. The tree's root should be an immediate child of
--  either the source or the target.
syncTrees
   :: (Monoid b)
   => JoinStrategy b
   -> LeftRoot
   -> RightRoot
   -> T.Tree (TreeDiff, FilePath)
   -> IO b
syncTrees strategy src trg = go ""
   where
      go path node@(T.Node (_,x) xs) = do
         (continue, res) <- strategy src trg path node
         if continue then mappend res <$> mconcat <$> mapM (go $ path </> x) xs
                     else return res

-- |Takes two directories and synchronizes them using a given join
--  strategy. Everything said about 'syncTrees' applies.
syncDirectories
   :: Monoid b
   => JoinStrategy b
   -> LeftRoot
   -> RightRoot
   -> IO b
syncDirectories strategy src trg = syncTrees strategy src trg =<< createDiffTree src trg

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module System.IO.FileSync.Sync where

import Data.List
import Data.Ord
import Data.Functor.Monadic
import qualified Data.Tree as T
import System.Directory
import System.FilePath

import System.IO.FileSync.Join
import System.IO.FileSync.Types

-- |Takes a root directory and creates a tree representing its structure,
--  starting with the immediate children.
createFileTree
   :: FileRoot src
   => src
   -> IO (T.Forest FilePath)
createFileTree src = T.subForest <$> go "" (getFilePath src)
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
--  The roots themselves will not be included in the resultant tree,
--  only their children.
createDiffTree
   :: LeftRoot
   -> RightRoot
   -> IO (T.Forest (TreeDiff, FilePath))
createDiffTree src trg = genericJoin <$> createFileTree src <*> createFileTree trg

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

-- |See 'syncTrees'. Works with forests.
syncForests
   :: (Monoid b)
   => JoinStrategy b
   -> LeftRoot
   -> RightRoot
   -> T.Forest (TreeDiff, FilePath)
   -> IO b
syncForests strategy src trg = mapM (syncTrees strategy src trg) >=$> mconcat

-- |Takes two directories and synchronizes them using a given join
--  strategy. Everything said about 'syncTrees' applies.
syncDirectories
   :: Monoid b
   => JoinStrategy b
   -> LeftRoot
   -> RightRoot
   -> IO b
syncDirectories strategy src trg =
   createDiffTree src trg
   >>= syncForests strategy src trg

-- |Recursively sorts a forest according to the keys.
sortForest :: Ord a => T.Forest a -> T.Forest a
sortForest = sortBy (comparing T.rootLabel) . map sortTree
   where
      sortTree (T.Node x xs) = T.Node x (sortForest xs)

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module System.IO.FileSync.Sync where

import Control.Monad.Trans.Either
import Control.Monad.Writer
import qualified Data.Conduit as Con
import Data.List
import Data.Ord
import qualified Data.Foldable as F
import qualified Data.Sequence as S
import qualified Data.Tree as T
import qualified Data.Tree.Monadic as Mt
import System.Directory
import System.FilePath
import qualified System.IO.Error as Err

import System.IO.FileSync.Join
import System.IO.FileSync.Types

-- import Debug.Trace

-- |Takes a root directory and creates a tree representing its structure,
--  starting with the immediate children.
--
--  Will create a one-element forest if the root is a file.
--
--  If a file/directory is moved during the operation, an 'Err.doesNotExist'
--  IO error is thrown.
createFileTree
   :: FileRoot src
   => src
   -> IO [Mt.MTree IO FileTreeData]
createFileTree src = Mt.subForest $ go "" (getFilePath src)
   where
      go root this = Mt.MTree $ do
         -- traceM ("[createFileTree")
         let isFile x = doesFileExist (root </> this </> x) >>= return . (,x)
         thisIsFile <- doesFileExist (root </> this)
         thisIsDir <- doesDirectoryExist (root </> this)
         if thisIsFile then return (FTD this File, [Mt.MTree $ return (FTD (takeFileName this) File, [])])
         else if thisIsDir then do
            -- traceM $ "[createFileTree] getting directory contents of: " ++ (root </> this)
            contents <- getDirectoryContents (root </> this)
            -- traceM $ "[createFileTree] success"
            (files, dirs) <- partition fst <$> mapM isFile contents
            let files' = map (\(_,x) -> Mt.MTree $ return (FTD x File, [])) files
                dirs' = filter (not . flip elem [".",".."]) . map snd $ dirs
                children = map (go $ root </> this) dirs'
            return (FTD this Directory, files' ++ children)
         else ioError $ Err.mkIOError
                           Err.doesNotExistErrorType
                           (root </> this)
                           Nothing
                           (Just $ root </> this)

-- |Creates a difference tree between two directories.
--  The roots themselves will not be included in the resultant tree,
--  only their children.
--
--  If identically named entries occur as directories in one directory
--  in one root and as files in another, a list of conflicts will be returned instead.
createDiffTree
   :: LeftRoot
   -> RightRoot
   -> EitherT (S.Seq FileDirectoryConflict) IO (T.Forest (FileTreeData, TreeDiff))
createDiffTree src trg = do
   let mat = (>>= mapM Mt.materialize)
   forest <- lift $ genericJoin <$> mat (createFileTree src) <*> mat (createFileTree trg)
   let (cleanedForest, errors) = runWriter $ reforest recordKeyConflicts forest
       mkError xs =
          FileDirectoryConflict (_fileTreeDataPath . head . F.toList $ xs)

   if S.null errors then right cleanedForest
                    else left . fmap mkError $ errors

-- |Takes two root directories and synchronizes the tree that starts in them
--  using a given strategy. The tree's root should be an immediate child of
--  either the source or the target.
syncTrees
   :: JoinStrategy b
   -> LeftRoot
   -> RightRoot
   -> T.Tree (FileTreeData, TreeDiff)
   -> Con.Source IO b
syncTrees strategy src trg = go ""
   where
      throughput [] = return Yes
      throughput (Right x:xs) = Con.yield x >> throughput xs
      throughput (Left x:_) = {- trace ("[syncTrees.throughput]" ++ show x) $ -} return x

      go path node@(T.Node (FTD x _,_) xs) = do
         continue <- strategy src trg path node
         when (continue == Yes) $ mapM_ (go $ path </> x) xs

-- |See 'syncTrees'. Works with forests.
syncForests
   :: JoinStrategy b
   -> LeftRoot
   -> RightRoot
   -> T.Forest (FileTreeData, TreeDiff)
   -> Con.Source IO b
syncForests strategy src trg = mapM_ (syncTrees strategy src trg)

-- |Takes two directories and synchronizes them using a given join
--  strategy. Everything said about 'syncTrees' applies.
--
--  The synchronization will only be performed if you read out the results (e.g.
--  via 'Data.Conduit.runConduit' or 'Data.Conduit.sourceToList').
syncDirectories
   :: JoinStrategy b
   -> LeftRoot
   -> RightRoot
   -> EitherT (S.Seq FileDirectoryConflict) IO (Con.Source IO b)
syncDirectories strategy src trg = syncForests strategy src trg <$> createDiffTree src trg

-- |Recursively sorts a forest according to the keys.
sortForest :: Ord a => T.Forest a -> T.Forest a
sortForest = sortBy (comparing T.rootLabel) . map sortTree
   where
      sortTree (T.Node x xs) = T.Node x (sortForest xs)

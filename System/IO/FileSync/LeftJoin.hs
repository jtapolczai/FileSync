{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module System.IO.FileSync.LeftJoin where

import Control.Arrow ((&&&))
import Data.Functor
import Data.List
import qualified Data.Map as M
import qualified Data.Tree as T
import System.Directory
import System.FilePath

--import Debug.Trace
import Debug.Trace.Disable

data TreeDiff = LeftOnly | RightOnly | Both
   deriving (Show, Eq, Ord, Read)

data EntryType = Directory | File
   deriving (Show, Eq, Ord, Read)


type JoinStrategy =
   FilePath
   -> FilePath
   -> FilePath
   -> T.Tree (TreeDiff, (FilePath, EntryType))
   -> IO Bool

-- |Generic join that computer the set of differences between two forests.
--  Ordering of subtrees is __not__ guaranteed. Subtrees with identical
--  root trees are merged.
genericJoin
   :: (Ord a, Show a)
   => T.Forest a -- ^The left forest S.
   -> T.Forest a -- ^The right forest T.
   -> T.Forest (TreeDiff, a)
genericJoin ts ss =
   map (terminate LeftOnly) leftOnly
   ++ map (terminate RightOnly) rightOnly
   ++ map continue both
   where
      toMap = M.fromListWith (\x y -> fmap snd $ head $ outerJoin [x] [y])
              . map (T.rootLabel &&& id)

      -- convert lists of children to maps for effient difference/intersection
      ssMap = toMap ss
      tsMap = toMap ts

      -- recurse for all keys that occur in both forests.
      -- end by inserting leftTag/rightTag for those which only occur in one.
      rightOnly = map snd $ M.toList $ M.difference tsMap ssMap
      leftOnly = map snd $ M.toList $ M.difference ssMap tsMap
      both = map snd $ M.toList $ M.intersectionWith pairChildren ssMap tsMap

      pairChildren (T.Node x xs) (T.Node _ ys) = (x,xs,ys)

      continue (x,xs,ys) = T.Node (Both, x) $ leftJoin xs ys
      terminate tag (T.Node x xs) = T.Node (tag, x) $ map (fmap (Both,)) xs

leftJoin :: JoinStrategy
leftJoin source target path (T.Node (LeftOnly, (fp, et))) =
   applyInsertAction source target (path </> fp) et
leftJoin source target path (T.Node (RightOnly, (fp, et))) =
   applyDeleteAction target (path </> fp) et
leftJoin source target path (T.Node (Both, (fp, et))) = return True

applyDeleteAction
   :: FilePath -- ^Path from which to start (generically a drive or somesuch).
   -> FilePath -- ^Path in the tree, starting from the root.
   -> EntryType
   -> IO ()
applyDeleteAction start path = undefined
   where
      path = start </> paths

applyInsertAction
   :: FilePath -- ^Prefix of the source path.
   -> FilePath -- ^Prefix of the target path.
   -> FilePath -- ^Path in the tree, starting from the root.
   -> EntryType
   -> IO ()
applyInsertAction source target path et = undefined
   where
      sPath = source </> path
      tPath = target </> path

createFileTree
   :: FilePath
   -> IO (T.Tree (FilePath, EntryType))
createFileTree = go ""
   where
      go root this =  do
         thisType <- (\case{True -> File; False -> Directory})
                     <$> doesFileExist (root </> this)
         let isFile x = doesFileExist (root </> this </> x) >>= return . (,x)
         contents <- getDirectoryContents (root </> this)
         (files, dirs) <- partition fst <$> mapM isFile contents
         let files' = map (\(_,x) -> T.Node (x,File) []) files
             dirs' = filter (not . flip elem [".",".."]) . map snd $ dirs
         children <- mapM (go $ root </> this) dirs'
         return $ T.Node (this, thisType) $ files' ++ children

syncWith
   :: JoinStrategy
   -> FilePath
   -> FilePath
   -> T.Tree (TreeDiff, a)
   -> IO ()
syncWith identHandler source target = undefined -- go
{-   where
      go (T.Node (T.Node (Identical, fp))) = do
         continue <- identHandler  -}


t1 = T.Node 1 []
t2 = T.Node 2 []
t3 = T.Node 3 []

t4 = T.Node 5 [t1, t2]
t5 = T.Node 5 [t2, t3]

t6 = T.Node 5 [T.Node 1 [T.Node 3 [T.Node 4 []]], T.Node 2 []]
t7 = T.Node 5 [T.Node 1 [], T.Node 2 [T.Node 6 [T.Node 7 []]]]

trace' x = traceShow x $ x

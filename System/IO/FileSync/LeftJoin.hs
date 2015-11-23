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

newtype LeftRoot = LR FilePath
   deriving (Show, Eq, Ord)

newtype RightRoot = RR FilePath
   deriving (Show, Eq, Ord)

class FileRoot a where
   getFilePath :: a -> FilePath

instance FileRoot LeftRoot where getFilePath (LR fp) = fp
instance FileRoot RightRoot where getFilePath (RR fp) = fp

type JoinStrategy a =
   LeftRoot
   -> RightRoot
   -> FilePath
   -> T.Tree (TreeDiff, FilePath)
   -> IO (Bool, a)

data FileAction = Delete FilePath | Copy FilePath FilePath
   deriving (Show, Eq, Ord)

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
      toMap = M.fromListWith (\x y -> fmap snd $ head $ genericJoin [x] [y])
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

      continue (x,xs,ys) = T.Node (Both, x) $ genericJoin xs ys
      terminate tag (T.Node x xs) = T.Node (tag, x) $ map (fmap (Both,)) xs

leftJoin :: JoinStrategy (IO ())
leftJoin left right path (T.Node (LeftOnly, fp) _) =
   return (False, applyInsertAction left right $ path </> fp)
leftJoin _ right path (T.Node (RightOnly, fp) _) =
   return (False, applyDeleteAction right $ path </> fp)
leftJoin _ _ _ _ = return (True, return ())

rightJoin :: JoinStrategy (IO ())
rightJoin left _ path (T.Node (LeftOnly, fp) _) =
   return (False, applyDeleteAction left $ path </> fp)
rightJoin left right path (T.Node (RightOnly, fp) _) =
   return (False, applyInsertAction right left $ path </> fp)
rightJoin _ _ _ _ = return (True, return ())

innerJoin :: JoinStrategy (IO ())
innerJoin left _ path (T.Node (LeftOnly, fp) _) =
   return (False, applyDeleteAction left $ path </> fp)
innerJoin _ right path (T.Node (RightOnly, fp) _) =
   return (False, applyDeleteAction right $ path </> fp)
innerJoin _ _ _ _ = return (True, return ())

outerJoin :: JoinStrategy (IO ())
outerJoin left right path (T.Node (LeftOnly, fp) _) =
   return (False, applyInsertAction left right $ path </> fp)
outerJoin left right path (T.Node (RightOnly, fp) _) =
   return (False, applyInsertAction right left $ path </> fp)
outerJoin _ _ _ _ = return (True, return ())

-- |Deletes a 
applyDeleteAction
   :: FileRoot src
   => src
   -> FilePath -- ^Path in the tree, starting from the roots.
   -> IO ()
applyDeleteAction root path = undefined
   where
      --path = start </> paths

applyInsertAction
   :: (FileRoot src, FileRoot trg)
   => src
   -> trg
   -> FilePath -- ^Path in the tree, starting from the roots.
   -> IO ()
applyInsertAction source target path = undefined
   where
      sPath = getFilePath source </> path
      tPath = getFilePath target </> path

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
   :: JoinStrategy b
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

{-# LANGUAGE TupleSections #-}

module System.IO.FileSync.LeftJoin where

import Control.Arrow ((&&&))
import Data.List
import qualified Data.Map as M
import qualified Data.Tree as T
import System.Directory
import System.FilePath

--import Debug.Trace
import Debug.Trace.Disable

-- |A possibly empty tree.
type Tree a = Maybe (T.Tree a)

data TreeDiff = Insert | Delete | Identical
   deriving (Show, Eq, Ord, Read, Enum)

-- |Generic join that computer the set of differences between two forests.
--  We take two forests, S and T. First, trees with identical root keys
--  __within__ S and __within__ T are united via a full outer join.
--
-- Then, we match up the trees from S with the trees from T. If a given root
-- key only occurs in the first forest, it's counted as "left only". If it only
-- occurs in the right one, it's counted as "right only". Left only trees
-- are inserted as-is into the result, with the left tag. Same for the right
-- only trees. For trees with identical root keys, we insert one tree
-- with the 'TreeDiff'-value 'Identical' in the root. In its children, we
-- repeat the 'join' function.
--
-- All other joins ('leftJoin', 'rightJoin', 'innerJoin', 'outerJoin') are
-- specialisations of this function.
join
   :: (Ord a, Show a)
   => TreeDiff -- ^Left tag, for trees/nodes that only occur in S.
   -> TreeDiff -- ^Right tag, for trees/nodes that only occur in T.
   -> T.Forest a -- ^The left forest S.
   -> T.Forest a -- ^The right forest T.
   -> T.Forest (TreeDiff, a)
join leftTag rightTag ts ss =
   map (terminate leftTag) leftOnly
   ++ map (terminate rightTag) rightOnly
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

      continue (x,xs,ys) = T.Node (Identical, x) $ leftJoin xs ys
      terminate tag (T.Node x xs) = T.Node (tag, x) $ map (fmap (Identical,)) xs

leftJoin :: (Ord a, Show a) => T.Forest a -> T.Forest a -> T.Forest (TreeDiff,a)
leftJoin = join Insert Delete

rightJoin :: (Ord a, Show a) => T.Forest a -> T.Forest a -> T.Forest (TreeDiff,a)
rightJoin = flip leftJoin

innerJoin :: (Ord a, Show a) => T.Forest a -> T.Forest a -> T.Forest (TreeDiff,a)
innerJoin = join Delete Delete

outerJoin :: (Ord a, Show a) => T.Forest a -> T.Forest a -> T.Forest (TreeDiff,a)
outerJoin = join Insert Insert

(<//>) root paths = if null paths then root else root </> foldl1' (</>) paths

applyDeleteAction
   :: FilePath -- ^Path from which to start (generically a drive or somesuch).
   -> [FilePath] -- ^Paths in the tree, starting from the root.
   -> IO ()
applyDeleteAction start paths = undefined
   where
      path = start <//> paths

applyInsertAction
   :: FilePath -- ^Prefix of the source path.
   -> FilePath -- ^Prefix of the target path.
   -> [FilePath] -- ^Paths in the tree, starting from the root.
   -> IO ()
applyInsertAction source target paths = undefined
   where
      sPath = source <//> paths
      tPath = target <//> paths



t1 = T.Node 1 []
t2 = T.Node 2 []
t3 = T.Node 3 []

t4 = T.Node 5 [t1, t2]
t5 = T.Node 5 [t2, t3]

t6 = T.Node 5 [T.Node 1 [T.Node 3 [T.Node 4 []]], T.Node 2 []]
t7 = T.Node 5 [T.Node 1 [], T.Node 2 [T.Node 6 [T.Node 7 []]]]

trace' x = traceShow x $ x

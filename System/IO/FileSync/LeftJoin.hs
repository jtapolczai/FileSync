{-# LANGUAGE TupleSections #-}

module System.IO.FileSync.LeftJoin where

import Control.Arrow ((&&&))
import qualified Data.Map as M
import qualified Data.Tree as T

--import Debug.Trace
import Debug.Trace.Disable

-- |A possibly empty tree.
type Tree a = Maybe (T.Tree a)

data TreeDiff = Insert | Delete | Identical
   deriving (Show, Eq, Ord, Read, Enum)

leftJoin :: (Ord a, Show a) => T.Forest a -> T.Forest a -> T.Forest (TreeDiff, a)
leftJoin ts ss =
   trace "We begin:"
   $ traceShow ts
   $ traceShow ss
   $ trace "-----------------"

   $ map (terminate Insert) leftOnly
                 ++ map (terminate Delete) rightOnly
                 ++ map continue both
   where
      -- convert lists of children to maps for effient difference/intersection
      tsMap = M.fromList (map (T.rootLabel &&& id) ts)
      ssMap = M.fromList (map (T.rootLabel &&& id) ss)

      -- delete all right only children, insert all left only ones,
      -- recurse over children which occur in both lists
      rightOnly = trace' $ trace "leftOnly" $ map snd $ M.toList $ M.difference ssMap tsMap
      leftOnly = trace' $ trace "rightOnly" $ map snd $ M.toList $ M.difference tsMap ssMap
      both = trace' $ trace "both" $ map snd $ M.toList $ M.intersectionWith pairChildren tsMap ssMap

      pairChildren (T.Node x xs) (T.Node _ ys) = (x,xs,ys)

      continue (x,xs,ys) = T.Node (Identical, x)
                           $ leftJoin xs ys
      terminate tag (T.Node x xs) = T.Node (tag, x)
                                    $ map (fmap (Identical,)) xs

t1 = T.Node 1 []
t2 = T.Node 2 []
t3 = T.Node 3 []

t4 = T.Node 5 [t1, t2]
t5 = T.Node 5 [t2, t3]

t6 = T.Node 5 [T.Node 1 [T.Node 3 [T.Node 4 []]], T.Node 2 []]
t7 = T.Node 5 [T.Node 1 [], T.Node 2 [T.Node 6 [T.Node 7 []]]]

trace' x = traceShow x $ x

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module System.IO.FileSync.Join where

import Control.Arrow ((&&&))
import qualified Data.Map as M
import qualified Data.Tree as T

import System.IO.FileSync.Types

-- |Generic join that computer the set of differences between two forests.
--  Ordering of subtrees is __not__ guaranteed. Subtrees with identical
--  root trees are merged.
genericJoin
   :: (Ord a, Show a)
   => T.Forest a -- ^The left forest S.
   -> T.Forest a -- ^The right forest T.
   -> T.Forest (a, TreeDiff)
genericJoin ss ts =
   map (terminate LeftOnly) leftOnly
   ++ map (terminate RightOnly) rightOnly
   ++ map continue both
   where
      toMap = M.fromListWith (\x y -> fmap fst $ head $ genericJoin [x] [y])
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

      continue (x,xs,ys) = T.Node (x, Both) $ genericJoin xs ys
      terminate tag (T.Node x xs) = T.Node (x, tag) $ map (fmap (,tag)) xs

-- |Filters out all sub-trees whose roots fail a predicate.
filterForest :: (a -> Bool) -> T.Forest a -> T.Forest a
filterForest pred = map filterForest' . filter (pred . T.rootLabel)
   where
      filterForest' (T.Node x xs) = T.Node x $ filterForest pred xs

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module System.IO.FileSync.Join where

import qualified Data.Foldable as F
import Data.List
import qualified Data.Map as M
import qualified Data.Sequence as S
import qualified Data.Set as St
import qualified Data.Tree as T

import System.IO.FileSync.Types

-- |Generic join that computer the set of differences between two forests.
--  Ordering of subtrees is __not__ guaranteed. Subtrees with identical
--  roots (according to '==') are merged.
genericJoin
   :: forall a.(LooseEq a, Ord a, Ord (Reduct a), Show a, Ord (T.Tree a))
   => T.Forest a -- ^The left forest S.
   -> T.Forest a -- ^The right forest T.
   -> T.Forest (St.Set a, TreeDiff) -- ^Joined forest. Values equal according to '=~='
                                    --  but __not__ according to '==' are grouped.
genericJoin ss ts = 
   map snd . M.toList . fmap recurse . groupChildren $ rawChildren
   where
      rawChildren = map (,LeftOnly) ss ++ map (,RightOnly) ts

      groupChildren :: [(T.Tree a, TreeDiff)]
                    -> EqClasses (Reduct a) (T.Tree a, TreeDiff)
      groupChildren = foldl' f $ M.empty
         where
            f acc val =
               M.insertWith (\_ old -> St.insert val old)
                            (reduce . T.rootLabel . fst $ val)
                            (St.singleton val)
                            acc

      recurse :: St.Set (T.Tree a, TreeDiff) -> T.Tree (St.Set a, TreeDiff)
      recurse = mkNode . F.foldl' f (St.empty, Both, S.empty, S.empty)
         where
            mkNode (x, side, ls, rs) =
               T.Node (x, side) $ genericJoin (F.toList ls) (F.toList rs) 

            f (vals, accSide, ls, rs) (T.Node x xs, side) =
               (St.insert x vals,
                if accSide == side then side else Both,
                if side == LeftOnly then S.fromList xs S.>< ls else ls,
                if side == RightOnly then S.fromList xs S.>< rs else rs)

-- |Filters out all sub-trees whose roots fail a predicate.
filterForest :: (a -> Bool) -> T.Forest a -> T.Forest a
filterForest pred = map filterForest' . filter (pred . T.rootLabel)
   where
      filterForest' (T.Node x xs) = T.Node x $ filterForest pred xs

-- |Applies a function to every node of a forest. Iff the function fails
--  for any node of any tree, Nothing is returned.
reforest :: (a -> Maybe b) -> T.Forest a -> Maybe (T.Forest b)
reforest f = mapM (traverse f)

-- |Returns Nothing iff the length of the 'FileTreeData'-list is @/=1@,
--  which indicates that the same entry names signifies a file in one tree and
--  a directory in another, meaning that the synching can't proceed.

-- |Tries to extract the first element of the list. Fails if the list
--  contains anything else than exactly 1 element.
onlyOneKey :: Eq a => ([a], b) -> Maybe (a, b)
onlyOneKey (xs,y) = if take 1 xs /= xs then Nothing else Just $ (head xs, y)

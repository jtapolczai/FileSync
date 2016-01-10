{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module System.IO.FileSync.Join where

import Control.Monad.Writer
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
   :: forall a.(LooseEq a, Ord a, Ord (Reduct a), Show a)
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
      groupChildren xs = foldl' f M.empty xs
         where
            f acc val =
               M.insertWith (\_ old -> old S.|> val)
                            (reduce . T.rootLabel . fst $ val)
                            (S.singleton val)
                            acc

      recurse :: F.Foldable f => f (T.Tree a, TreeDiff) -> T.Tree (St.Set a, TreeDiff)
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

-- |Applies a function to every node of a forest.
--
--  Use cases:
--
--  * Applying a Maybe-returing function to check some condition;
--  * Applying a Writer to record errors.
reforest :: Monad f => (a -> f b) -> T.Forest a -> f (T.Forest b)
reforest f = mapM (traverse f)

-- |Returns Nothing iff the length of the 'FileTreeData'-list is @/=1@,
--  which indicates that the same entry names signifies a file in one tree and
--  a directory in another, meaning that the synching can't proceed.

-- |Tries to extract the first element of the list. Fails if the list
--  contains anything else than exactly 1 element.
onlyOneKey :: Eq a => ([a], b) -> Maybe (a, b)
onlyOneKey ([x],y) = Just (x,y)
onlyOneKey _ = Nothing

-- |Records all instances in which there are multiple keys in a node.
--  The first two keys are put into an exception.
--
--  Will fail in case of 0-size key lists.
recordKeyConflicts
   :: (MonadWriter (S.Seq (St.Set a)) m, Eq a)
   => (St.Set a, b)
   -> m (a, b)
recordKeyConflicts (x,y) = go (St.toList x) y
   where
      go [] _ = error "recordKeyConflicts: empty key list."
      go [x1] y = return (x1,y)
      go (x1:_) y = tell (S.singleton x) >> return (x1, y)

module System.IO.FileSync.LeftJoin where

import qualified Data.Tree as T

-- |A possibly empty tree.
type Tree a = Maybe (T.Tree a)

data TreeDiff a = Insert (T.Tree a) | Delete (T.Tree a)

leftJoin :: Tree a -> Tree a -> Tree (TreeDiff a)
leftJoin Nothing _ = Nothing
leftJoin (Just t) Nothing =  Just $ T.Node (Insert t) []
leftJoin (Just t) (Just s) = undefined

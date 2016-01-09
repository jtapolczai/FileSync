{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module System.IO.FileSync.Types where

import Data.Char
import qualified Data.Map as M
import qualified Data.Set as St
import qualified Data.Tree as T

-- |Loose equality which may equate different values.
--
--  The following should hold:
--
-- @
--  (x == y) ==> (x =~= y)
-- @
--
-- @
-- (reduce a == reduce b) <==> (a =~= b)
-- @
--
-- Ideally, instances should also form equivalence classes (i.e. =~= should
-- be reflexive, symmetric and transitive).
class Eq a => LooseEq a where
   -- |Loose comparison of values.
   --  The default implementation is '=='.
   (=~=) :: a -> a -> Bool
   (=~=) = (==)

   type Reduct a :: *
   type instance Reduct a = a

   -- |Gets the reduct of a value - the fragment which really decides equality in the sense of '=='.
   --  The default implementation is 'id'.
   reduce :: a -> Reduct a

-- |A collection of equivalence classes, with each class represented by a single element.
type EqClasses k v = M.Map k (St.Set v)

data TreeDiff = LeftOnly | RightOnly | Both
   deriving (Show, Eq, Ord, Read)

data FileTreeData = FTD
   {_fileTreeDataPath :: FilePath,
    _fileTreeDataType :: EntryType
   }
   deriving (Eq, Ord, Read)

instance Show FileTreeData where
   show (FTD fp t) = "FTD " ++ show fp ++ " " ++ show t

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
   -> T.Tree (FileTreeData, TreeDiff)
   -> IO (Bool, a)

type DifferenceHandler = FilePath -> IO (Maybe FileAction)

data RootSide = LeftSide | RightSide
   deriving (Show, Eq, Ord, Enum, Bounded, Read)

data FileAction = Delete RootSide FilePath | Copy RootSide FilePath
   deriving (Show, Eq, Ord, Read)

data YesNo = Yes | No
   deriving (Show, Eq, Ord, Enum, Bounded)

-- |Valid values are 'y', 'Y', 'n', 'N'.
instance Read YesNo where
   readsPrec _ xs = case dropWhile isSpace xs of
      [] -> []
      (y:ys) -> maybe [] ((:[]) . (,ys))
                      $ lookup y [('y', Yes),
                                  ('Y', Yes),
                                  ('n', No),
                                  ('N', No)]

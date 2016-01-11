{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module System.IO.FileSync.Types where

import Control.Exception
import Data.Char
import qualified Data.Map as M
import qualified Data.Sequence as S
import qualified Data.Tree as T
import Data.Typeable

-- |Loose equality which may equate different values.
--
--  The following should hold:
--
-- @
--  (x == y) ==> (reduct x == reduct y)
-- @
--
-- Ideally, instances should also form equivalence classes (i.e. =~= should
-- be reflexive, symmetric and transitive).
class Eq a => LooseEq a where
   type Reduct a :: *
   type instance Reduct a = a

   -- |Gets the reduct of a value - the fragment which really decides equality in the sense of '=='.
   --  The default implementation is 'id'.
   reduce :: a -> Reduct a

-- |Loose comparison of values.
--
-- @
-- (x =~= y)   ===   (reduct x == reduct y)
-- @
(=~=) :: (Eq (Reduct a), LooseEq a) => a -> a -> Bool
(=~=) x y = reduce x == reduce y

-- |A collection of equivalence classes, with each class represented by a single element.
type EqClasses k v = M.Map k (S.Seq v)

data TreeDiff = LeftOnly | RightOnly | Both
   deriving (Show, Eq, Ord, Read)

data FileTreeData = FTD
   {_fileTreeDataPath :: FilePath,
    _fileTreeDataType :: EntryType
   }
   deriving (Eq, Ord, Read)

instance Show FileTreeData where
   show (FTD fp t) = "FTD " ++ show fp ++ " " ++ show t

-- |Loose equality is decided based on '_fileTreeDataPath'.
instance LooseEq FileTreeData where
   type Reduct FileTreeData = FilePath
   reduce (FTD fp _) = fp

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

-- Exceptions
-------------------------------------------------------------------------------

-- |Indicates that an entry is a file in one place and a directory in another.
data FileDirectoryConflict = FileDirectoryConflict{fromFTD :: FilePath} deriving (Show, Typeable)

instance Exception FileDirectoryConflict

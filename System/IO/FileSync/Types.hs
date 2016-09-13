{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- |Contains all the types needed by the rest of the package.
module System.IO.FileSync.Types where

import Control.Exception
import Data.Char
import qualified Data.Conduit as Con
import qualified Data.HashMap as HM
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
   reduct :: a -> Reduct a

-- |Loose comparison of values.
--
-- @
-- (x =~= y)   ===   (reduct x == reduct y)
-- @
(=~=) :: (Eq (Reduct a), LooseEq a) => a -> a -> Bool
(=~=) x y = reduct x == reduct y

-- |A collection of equivalence classes, with each class represented by a single element.
type EqClasses k v = M.Map k (S.Seq v)

-- |Indicates where a node is present on a difference tree.
data TreeDiff = LeftOnly | RightOnly | Both
   deriving (Show, Eq, Ord, Read)

-- |Node data for file trees.
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
   reduct (FTD fp _) = fp

-- |Indicates the kind of filesystem object.
data EntryType = Directory | File
   deriving (Show, Eq, Ord, Read)

-- |The left root path.
newtype LeftRoot = LR FilePath
   deriving (Show, Eq, Ord)

-- |The right root path.
newtype RightRoot = RR FilePath
   deriving (Show, Eq, Ord)

-- |Class of filepaths that are roots.
class FileRoot a where
   getFilePath :: a -> FilePath

instance FileRoot LeftRoot where getFilePath (LR fp) = fp
instance FileRoot RightRoot where getFilePath (RR fp) = fp

-- |A function which takes a part of a difference tree and returns a stream of file actions
--  which should be taken. The last returned value should be the Continue-field. If True,
--  the join will proceed down the subtrees. If not, it terminates for the given tree.
type JoinStrategy a =
   LeftRoot
   -> RightRoot
   -> FilePath
   -> T.Tree (FileTreeData, TreeDiff)
   -> Con.ConduitM () a IO Continue

-- |A difference handler for filepaths that are present in multiple locations.
--  These can optionally produce 'FileAction' or do nothing.
type DifferenceHandler = FilePath -> IO (Maybe FileAction)

-- |Left/right indicator for path roots.
data RootSide = LeftSide | RightSide
   deriving (Show, Eq, Ord, Enum, Bounded, Read)

-- |A syncing action to be taken. Either delete one side or copy over from one.
data FileAction = Delete RootSide FilePath | Copy RootSide FilePath
   deriving (Show, Eq, Ord, Read)

-- |Indicates whether the descent into a tree should continue.
type Continue = YesNo

-- |Yes or no.
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

-- Exclusions
-------------------------------------------------------------------------------

-- |A structured collection of excluded files and directories.
type Exclusions = SearchTree FilePath

-- |Search tree in which child nodes are stored in hashmaps.
data SearchTree a = SearchNode Bool (HM.Map a (SearchTree a))

-- |An action to be taken when filtering a forest.
data SearchAction = Exclude | KeepAndStop | KeepAndContinue
   deriving (Show, Eq, Ord, Enum)

-- Exceptions
-------------------------------------------------------------------------------

-- |Indicates that an entry is a file in one place and a directory in another.
data FileDirectoryConflict = FileDirectoryConflict{fromFTD :: FilePath} deriving (Show, Typeable)

instance Exception FileDirectoryConflict

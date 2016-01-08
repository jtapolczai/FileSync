{-# LANGUAGE TupleSections #-}

module System.IO.FileSync.Types where

import Data.Char
import qualified Data.Tree as T

data TreeDiff = LeftOnly | RightOnly | Both
   deriving (Show, Eq, Ord, Read)

data FileTreeData = FTD
   {_fileTreeDataPath :: FilePath,
    _fileTreeDataType :: EntryType
   }
   deriving (Eq, Show, Ord, Read)

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

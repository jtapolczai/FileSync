module System.IO.FileSync.Types where

import qualified Data.Tree as T

data TreeDiff = LeftOnly | RightOnly | Both
   deriving (Show, Eq, Ord, Read)

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
   -> T.Tree (TreeDiff, FilePath)
   -> IO (Bool, a)

data FileAction = Delete FilePath | Copy FilePath FilePath
   deriving (Show, Eq, Ord)

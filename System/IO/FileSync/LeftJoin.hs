{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module System.IO.FileSync.LeftJoin where

import Control.Arrow ((&&&))
import Control.Exception
import Data.List
import qualified Data.Map as M
import qualified Data.Tree as T
import System.Directory
import System.FilePath
import System.IO.Error

--import Debug.Trace
import Debug.Trace.Disable

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

-- |Generic join that computer the set of differences between two forests.
--  Ordering of subtrees is __not__ guaranteed. Subtrees with identical
--  root trees are merged.
genericJoin
   :: (Ord a, Show a)
   => T.Forest a -- ^The left forest S.
   -> T.Forest a -- ^The right forest T.
   -> T.Forest (TreeDiff, a)
genericJoin ts ss =
   map (terminate LeftOnly) leftOnly
   ++ map (terminate RightOnly) rightOnly
   ++ map continue both
   where
      toMap = M.fromListWith (\x y -> fmap snd $ head $ genericJoin [x] [y])
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

      continue (x,xs,ys) = T.Node (Both, x) $ genericJoin xs ys
      terminate tag (T.Node x xs) = T.Node (tag, x) $ map (fmap (Both,)) xs

leftJoin :: JoinStrategy (IO ())
leftJoin left right path (T.Node (LeftOnly, fp) _) =
   return (False, applyInsertAction left right $ path </> fp)
leftJoin _ right path (T.Node (RightOnly, fp) _) =
   return (False, applyDeleteAction right $ path </> fp)
leftJoin _ _ _ _ = return (True, return ())

rightJoin :: JoinStrategy (IO ())
rightJoin left _ path (T.Node (LeftOnly, fp) _) =
   return (False, applyDeleteAction left $ path </> fp)
rightJoin left right path (T.Node (RightOnly, fp) _) =
   return (False, applyInsertAction right left $ path </> fp)
rightJoin _ _ _ _ = return (True, return ())

innerJoin :: JoinStrategy (IO ())
innerJoin left _ path (T.Node (LeftOnly, fp) _) =
   return (False, applyDeleteAction left $ path </> fp)
innerJoin _ right path (T.Node (RightOnly, fp) _) =
   return (False, applyDeleteAction right $ path </> fp)
innerJoin _ _ _ _ = return (True, return ())

outerJoin :: JoinStrategy (IO ())
outerJoin left right path (T.Node (LeftOnly, fp) _) =
   return (False, applyInsertAction left right $ path </> fp)
outerJoin left right path (T.Node (RightOnly, fp) _) =
   return (False, applyInsertAction right left $ path </> fp)
outerJoin _ _ _ _ = return (True, return ())

-- |Deletes a file or directory. Does not handle exceptions.
--  This function tries to remove the target of the given path
--  as a directory. If that fails, it tries to remove it as a
--  file.
--
--  The path to remove is given by @S </> P@.
applyDeleteAction
   :: FileRoot src
   => src -- |Root S of the source.
   -> FilePath -- ^Path P in the tree, starting from the root.
   -> IO ()
applyDeleteAction src path =
   catchJust noDirFoundException
            (removeDirectoryRecursive sPath)
            (const $ removeFile sPath)
   where
      sPath = getFilePath src </> path

-- |Copies a file or directory. This funciton tries to
--  to copy the given path as a directory. If that fails,
--  it tries to copy it as a file.
--
--  The source is given by @S </> P@. The target is given
--  by @T </> P@.
applyInsertAction
   :: (FileRoot src, FileRoot trg)
   => src -- |Root S of the source.
   -> trg -- |Root T of the target.
   -> FilePath -- ^Path P in the tree, starting from the roots.
   -> IO ()
applyInsertAction src trg path = 
   catchJust noDirFoundException
             (copyDirectory sPath tPath)
             (const $ copyFile sPath tPath)
   where
      sPath = getFilePath src </> path
      tPath = getFilePath trg </> path

-- |Copies a directory recursively. Tries to copy permissions.
--  Does not handle exceptions.
copyDirectory :: FilePath -> FilePath -> IO ()
copyDirectory src trg = go ""
   where
      go path = getDirectoryContents (src </> path) >>= mapM_ (tryCopyRec path)

      tryCopyRec :: FilePath -> FilePath -> IO ()
      tryCopyRec _ "." = return ()
      tryCopyRec _ ".." = return ()
      tryCopyRec path cur = let
            sPath = src </> path </> cur
            tPath = trg </> path </> cur
         in
            catchJust noDirFoundException
                     (do createDirectory tPath
                         copyPermissions sPath tPath
                         go $ path </> cur)
                     (\_ -> do copyFile sPath tPath
                               copyPermissions sPath tPath)


-- |Returns a Just iff the exception is of type "DoesNotExist"/"NoSuchThing".
noDirFoundException :: IOError -> Maybe ()
noDirFoundException e =
   if isDoesNotExistErrorType (ioeGetErrorType e)
   then Just () else Nothing

-- |Takes a root directory and creates a tree representing its structure.
createFileTree
   :: FileRoot src
   => src
   -> IO (T.Tree FilePath)
createFileTree = go "" . getFilePath
   where
      go root this =  do
         let isFile x = doesFileExist (root </> this </> x) >>= return . (,x)
         contents <- getDirectoryContents (root </> this)
         (files, dirs) <- partition fst <$> mapM isFile contents
         let files' = map (\(_,x) -> T.Node x []) files
             dirs' = filter (not . flip elem [".",".."]) . map snd $ dirs
         children <- mapM (go $ root </> this) dirs'
         return $ T.Node this $ files' ++ children

-- |Creates a difference tree between two directories.
createDiffTree
   :: LeftRoot
   -> RightRoot
   -> IO (T.Tree (TreeDiff, FilePath))
createDiffTree src trg = (\s t -> head $ genericJoin [s] [t]) <$> createFileTree src <*> createFileTree trg

-- |Takes two root directories and synchronizes the tree that starts in them
--  using a given strategy. The tree's root should be an immediate child of
--  either the source or the target.
syncTrees
   :: (Monoid b)
   => JoinStrategy b
   -> LeftRoot
   -> RightRoot
   -> T.Tree (TreeDiff, FilePath)
   -> IO b
syncTrees strategy src trg = go ""
   where
      go path node@(T.Node (_,x) xs) = do
         (continue, res) <- strategy src trg path node
         if continue then mappend res <$> mconcat <$> mapM (go $ path </> x) xs
                     else return res

-- |Takes two directories and synchronizes them using a given join
--  strategy. Everything said about 'syncTrees' applies.
syncDirectories
   :: Monoid b
   => JoinStrategy b
   -> LeftRoot
   -> RightRoot
   -> IO b
syncDirectories strategy src trg = syncTrees strategy src trg =<< createDiffTree src trg


t1 = T.Node 1 []
t2 = T.Node 2 []
t3 = T.Node 3 []

t4 = T.Node 5 [t1, t2]
t5 = T.Node 5 [t2, t3]

t6 = T.Node 5 [T.Node 1 [T.Node 3 [T.Node 4 []]], T.Node 2 []]
t7 = T.Node 5 [T.Node 1 [], T.Node 2 [T.Node 6 [T.Node 7 []]]]

trace' x = traceShow x $ x

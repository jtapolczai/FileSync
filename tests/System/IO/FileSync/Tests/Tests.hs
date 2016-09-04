-- |Test data and common helper functions for tests.
module System.IO.FileSync.Tests.Tests where

import Control.Exception
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as St
import Data.Time.Calendar
import Data.Time.Clock
import qualified Data.Tree as Tr
import System.Directory
import System.IO
import System.FilePath hiding ((</>))
 -- we overwrite </> so that it only inserts slashes (/)
 -- apparently, Windows has a problem with paths that mix / and \\.
import qualified System.FilePath as FP
import Test.HUnit

import System.IO.FileSync.Types

data TwoKey = TK String String deriving (Show, Eq, Ord)

instance LooseEq TwoKey where
   type Reduct TwoKey = String
   reduct (TK s _) = s

-- |Utility to turn \\ into /. Windows' usage of \\ as path separator
--  causes test failures.
replaceSlashes :: Functor f => f FileAction -> f FileAction
replaceSlashes = fmap r
   where
      r (Delete s p) = (Delete s $ map slashes p)
      r (Copy s p) = (Copy s $ map slashes p)

      slashes '\\' = '/'
      slashes x = x

(</>) :: FilePath -> FilePath -> FilePath
(</>) f1 f2 = map slashes (f1 FP.</> f2)
   where
      slashes '\\' = '/'
      slashes x = x

-- Test data
-------------------------------------------------------------------------------

testDirsL :: IO ()
testDirsL = do
   mkD "testDir/dir1"
   mkD "testDir/dir1/both"
   mkD "testDir/dir1/both/both1"
   mkF "testDir/dir1/both/both1" "both2.txt" "both2_left larger"
   setModTime 2009 01 01 "testDir/dir1/both/both1/both2.txt"
   mkF "testDir/dir1/both/both1" "both3.txt" "both3_left"
   setModTime 2010 01 01 "testDir/dir1/both/both1/both3.txt"
   mkD "testDir/dir1/both/both_onlyL"
   mkF "testDir/dir1/both/both_onlyL" "both_onlyL.txt" "both_onlyL"
   mkF "testDir/dir1/both/" "both.txt" "bothL"
   setModTime 2010 01 01 "testDir/dir1/both/both.txt"
   mkD "testDir/dir1/onlyL"
   mkD "testDir/dir1/onlyL/l1"
   mkD "testDir/dir1/onlyL/l1/l3"
   mkD "testDir/dir1/onlyL/l1/l3/l4"
   mkF "testDir/dir1/onlyL/l1/l3/l4" "l2txt.txt" "l2"
   mkF "testDir/dir1/onlyL/l1" "ltxt.txt" "ltxt"
   mkD "testDir/dir1/onlyL/l2"

testDirsR :: IO ()
testDirsR = do
   mkD "testDir/dir2"
   mkD "testDir/dir2/both"
   mkD "testDir/dir2/both/both1"
   mkF "testDir/dir2/both/both1" "both2.txt" "both2_right"
   setModTime 2010 01 01 "testDir/dir2/both/both1/both2.txt"
   mkF "testDir/dir2/both/both1" "both3.txt" "both3_right larger"
   setModTime 2009 01 01 "testDir/dir2/both/both1/both3.txt"
   mkD "testDir/dir2/both/both_onlyR"
   mkF "testDir/dir2/both/both_onlyR" "both_onlyR.txt" "both_onlyR"
   mkF "testDir/dir2/both/" "both.txt" "bothR"
   setModTime 2010 01 01 "testDir/dir2/both/both.txt"
   mkD "testDir/dir2/onlyR"
   mkD "testDir/dir2/onlyR/r1"
   mkD "testDir/dir2/onlyR/r2"
   mkD "testDir/dir2/onlyR/r2/r3"
   mkF "testDir/dir2/onlyR/r2/r3" "rtxt.txt" "rtxt"

ft1 :: Tr.Forest FileTreeData
ft1 = [Tr.Node (FTD "both" Directory)
         [Tr.Node (FTD "both1" Directory)
            [Tr.Node (FTD "both2.txt" File) [],
             Tr.Node (FTD "both3.txt" File) []],
          Tr.Node (FTD "both_onlyL" Directory)
             [Tr.Node (FTD "both_onlyL.txt" File) []],
          Tr.Node (FTD "both.txt" File) []],
       Tr.Node (FTD "onlyL" Directory)
          [Tr.Node (FTD "l1" Directory)
             [Tr.Node (FTD "l3" Directory)
                [Tr.Node (FTD "l4" Directory)
                   [Tr.Node (FTD "l2txt.txt" File) []]],
              Tr.Node (FTD "ltxt.txt" File) []],
           Tr.Node (FTD "l2" Directory) []]]

dt1 :: Tr.Forest (FileTreeData, TreeDiff)
dt1 = [Tr.Node (FTD "both" Directory, Both)
         [Tr.Node (FTD "both1" Directory, Both)
            [Tr.Node (FTD "both2.txt" File, Both) [],
             Tr.Node (FTD "both3.txt" File, Both) []],
          Tr.Node (FTD "both_onlyL" Directory, LeftOnly)
             [Tr.Node (FTD "both_onlyL.txt" File, LeftOnly) []],
          Tr.Node (FTD "both_onlyR" Directory, RightOnly)
             [Tr.Node (FTD "both_onlyR.txt" File, RightOnly) []],
          Tr.Node (FTD "both.txt" File, Both) []],
       Tr.Node (FTD "onlyL" Directory, LeftOnly)
          [Tr.Node (FTD "l1" Directory, LeftOnly)
             [Tr.Node (FTD "l3" Directory, LeftOnly)
                [Tr.Node (FTD "l4" Directory, LeftOnly)
                   [Tr.Node (FTD "l2txt.txt" File, LeftOnly) []]],
              Tr.Node (FTD "ltxt.txt" File, LeftOnly) []],
           Tr.Node (FTD "l2" Directory, LeftOnly) []],
       Tr.Node (FTD "onlyR" Directory, RightOnly)
          [Tr.Node (FTD "r1" Directory, RightOnly) [],
           Tr.Node (FTD "r2" Directory, RightOnly)
             [Tr.Node (FTD "r3" Directory, RightOnly)
                [Tr.Node (FTD "rtxt.txt" File, RightOnly) []]]]]

tr1 :: Tr.Tree Int
tr1 = Tr.Node 1 [l 4, l 3, l 1, l 8, Tr.Node 7 [l 1, l 9]]

tr2 :: Tr.Tree Int
tr2 = Tr.Node 1 [l 1, l 4, Tr.Node 7 [l 9, l 1], l 8, l 3]

tr3 :: Tr.Tree Int
tr3 = Tr.Node 1 [l 1, l 2, Tr.Node 5 [l 9, l 1], l 8, l 3]

tr4 :: Tr.Tree TwoKey
tr4 = Tr.Node (TK "A" "x")
              [Tr.Node (TK "B1" "a") [l (TK "alpha" ""), l (TK "alphaL" "")],
               Tr.Node (TK "B1" "b") [l (TK "betaGamma" ""), l (TK "beta" "")],
               Tr.Node (TK "B2" "") [l (TK "b2" "")]]

tr5 :: Tr.Tree TwoKey
tr5 = Tr.Node (TK "A" "y")
              [Tr.Node (TK "B1" "a") [l (TK "alpha" ""), l (TK "alphaR" "")],
               Tr.Node (TK "B1" "c") [l (TK "betaGamma" ""), l (TK "gamma" "")],
               Tr.Node (TK "B1" "d") [l (TK "delta" "")],
               Tr.Node (TK "C" "") [l (TK "c" "")]]

tr6 :: Tr.Tree (St.Set TwoKey, TreeDiff)
tr6 = Tr.Node (St.fromList [(TK "A" "x"), (TK "A" "y")], Both)
              [Tr.Node (St.fromList [(TK "B1" "a"),
                                     (TK "B1" "b"),
                                     (TK "B1" "c"),
                                     (TK "B1" "d")], Both)
                       [l (St.fromList [(TK "alpha" "")], Both),
                        l (St.fromList [(TK "alphaL" "")], LeftOnly),
                        l (St.fromList [(TK "alphaR" "")], RightOnly),
                        l (St.fromList [(TK "betaGamma" "")], Both),
                        l (St.fromList [(TK "beta" "")], LeftOnly),
                        l (St.fromList [(TK "gamma" "")], RightOnly),
                        l (St.fromList [(TK "delta" "")], RightOnly)],
               Tr.Node (St.fromList [(TK "B2" "")], LeftOnly)
                       [l (St.fromList [(TK "b2" "")], LeftOnly)],
               Tr.Node (St.fromList [(TK "C" "")], RightOnly)
                       [l (St.fromList [(TK "c" "")], RightOnly)]]

lr :: LeftRoot
lr = LR "testDir/dir1"

rr :: RightRoot
rr = RR "testDir/dir2"

-- Helpers
-------------------------------------------------------------------------------

-- |Shorthand for creating a leaf.
l :: a -> Tr.Tree a
l = flip Tr.Node []

-- |Shorthand for creating a directory (and its parents too) if it doesn't exist.
mkD :: FilePath -> IO ()
mkD = createDirectoryIfMissing True

-- |Shorthand for creating a text file in a directory. Creates the parents if they don't exist.
mkF :: FilePath -- ^Directory name
    -> FilePath -- ^Filename
    -> String -- ^Contents
    -> IO ()
mkF dir fn contents = mkD dir >> withFile (dir </> fn) WriteMode (flip hPutStr contents)

-- |Shorthand for removing a directory and all of its subdirectories. Warning: follows symlinks.
rmD :: FilePath -> IO ()
rmD = removeDirectoryRecursive

-- |Shorthand for a bracket that ignores the set-up's return value.
bracket' :: IO a -> IO b -> IO c -> IO c
bracket' start end op = bracket start (const end) (const op)

-- |Returns True iff the children of a given path match the elements of a forest.
--  For files, the given forest has to be empty.
directoryStructureMatches :: FilePath -> Tr.Forest FileTreeData -> IO Bool
directoryStructureMatches fp xs = do
   isFile <- doesFileExist fp
   isDir <- doesDirectoryExist fp
   if isFile then return $ null xs
   else if not isDir then return False
   else do
      -- traceM ("[directoryStructureMatches] Getting directory contents of: " ++ fp)
      contents <- filter (not . flip elem [".",".."]) <$> getDirectoryContents fp
      -- traceM ("[directoryStructureMatches] success.")
      dirs <- filterM doesDirectoryExist contents
      files <- St.fromList <$> filterM doesFileExist contents

      let leaves = St.fromList
                   . map (_fileTreeDataPath . Tr.rootLabel)
                   . filter ((File==) . _fileTreeDataType . Tr.rootLabel)
                   $ xs
          xsMap = M.fromList . map (\(Tr.Node (FTD y _) ys) -> (y,ys)) $ xs

          leavesMatch = files == leaves

          dirCall :: FilePath -> IO Bool
          dirCall d = maybe (return False) (directoryStructureMatches $ fp </> d) (M.lookup d xsMap)

      dirsMatch <- and <$> mapM dirCall dirs

      return $ leavesMatch && dirsMatch

-- |Sets the modification time of a file.
--  The date will be set to the one given, the time will be set to 00:00.
setModTime :: Integer -- ^Year
           -> Int -- ^Month
           -> Int -- ^Day
           -> FilePath
           -> IO ()
setModTime y m d fp =
   setModificationTime fp $ UTCTime (fromGregorian y m d) $ secondsToDiffTime 0

-- |Reads a text file and asserts that its contents should equal a given string.
assertContent :: String -> FilePath -> Assertion
assertContent content fp = do
   fileContent <- readFile fp
   assertEqual ("contents of file " ++ fp) content fileContent

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _ = error "fromRight: got a Left"

-------------------------------------------------------------------------------

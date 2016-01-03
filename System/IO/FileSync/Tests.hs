module System.IO.FileSync.Tests where

import Control.Exception
import qualified Data.Tree as Tr
import System.Directory
import System.IO
import System.FilePath
import Test.Hspec
import Test.Hspec.Contrib.HUnit
import Test.HUnit

import System.IO.FileSync.Sync
import System.IO.FileSync.Types

runTests :: IO ()
runTests = hspec . fromHUnitTest $ tests

tests = TestList
   [TestLabel "createEmptyFileTree" $ TestCase createEmptyFileTree,
    TestLabel "createFileTree1" $ TestCase createFileTree1,
    TestLabel "createEmptyDiffTree" $ TestCase createEmptyDiffTree,
    TestLabel "createDiffTree1" $ TestCase createDiffTree1,
    TestLabel "sortTree1" $ TestCase sortTree1,
    TestLabel "sortTree2" $ TestCase sortTree2,
    TestLabel "sortTree3" $ TestCase sortTree3,
    TestLabel "sortTree4" $ TestCase sortTree4,
    TestLabel "sortTree5" $ TestCase sortTree5,
    TestLabel "sortTree6" $ TestCase sortTree6,
    TestLabel "sortTree7" $ TestCase sortTree7]

-- Create file tree
-------------------------------------------------------------------------------

createEmptyFileTree :: Assertion
createEmptyFileTree =
   bracket' (mkD "testDir/empty")
            (rmD "testDir")
            (do t <- createFileTree (LR "testDir/empty")
                assertEqual "empty file tree" [] t)

createFileTree1 :: Assertion
createFileTree1 =
   bracket' testDirsL
            (rmD "testDir")
            (do t <- createFileTree (LR "testDir/dir1")
                putStrLn "Expected forest:"
                putStrLn . Tr.drawForest . map (fmap show) . sortForest $ ft1 
                putStrLn "Actual forest:"
                putStrLn . Tr.drawForest . map (fmap show) . sortForest $ t 
                assertEqual "file tree 1" (sortForest ft1) (sortForest t))

-- Create diff tree
-------------------------------------------------------------------------------

createEmptyDiffTree :: Assertion
createEmptyDiffTree =
   bracket' (mkD "testDir/empty")
            (rmD "testDir")
            (do t <- createDiffTree (LR "testDir/empty") (RR "testDir/empty")
                assertEqual "empty file tree" [] t)

createDiffTree1 :: Assertion
createDiffTree1 =
   bracket' (do testDirsL
                testDirsR)
            (rmD "testDir")
            (do t <- createDiffTree (LR "testDir/dir1") (RR "testDir/dir2")
                putStrLn "Expected forest:"
                putStrLn . Tr.drawForest . map (fmap show) . sortForest $ dt1
                putStrLn "Actual forest:"
                putStrLn . Tr.drawForest . map (fmap show) . sortForest $ t 
                assertEqual "diff tree 1" (sortForest dt1) (sortForest t))

-- Filter
-------------------------------------------------------------------------------

-- Sort forest
-------------------------------------------------------------------------------

sortTree1 :: Assertion
sortTree1 = assertEqual "empty forests" (sortForest fr) (sortForest fr)
   where
      fr :: Tr.Forest Int
      fr = []

sortTree2 :: Assertion
sortTree2 = assertBool "one empty and one non-empty forest"
                       $ sortForest [Tr.Node 1 []] /= sortForest []

sortTree3 :: Assertion
sortTree3 = assertEqual "the same forest twice" (sortForest dt1) (sortForest dt1)

sortTree4 :: Assertion
sortTree4 = assertEqual "equal forests mod. sorting" (sortForest [tr1]) (sortForest [tr2])

sortTree5 :: Assertion
sortTree5 = assertEqual "idempotent sorting" (sortForest [tr1]) (sortForest $ sortForest [tr1])

sortTree6 :: Assertion
sortTree6 = assertEqual "equal forests mod. sorting" (sortForest [tr1, tr2]) (sortForest [tr2, tr1])

sortTree7 :: Assertion
sortTree7 = assertBool "different trees (even mod. sorting)" $ sortForest [tr1] /= sortForest [tr3] 

-- Test data
-------------------------------------------------------------------------------

testDirsL :: IO ()
testDirsL = do
   mkD "testDir/dir1"
   mkD "testDir/dir1/both"
   mkD "testDir/dir1/both/both1"
   mkF "testDir/dir1/both/both1" "both2.txt" "both2_left"
   mkD "testDir/dir1/both/both_onlyL"
   mkF "testDir/dir1/both/both_onlyL" "both_onlyL.txt" "both_onlyL"
   mkF "testDir/dir1/both/" "both.txt" "both"
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
   mkD "testDir/dir2/both/both_onlyR"
   mkF "testDir/dir2/both/both_onlyR" "both_onlyR.txt" "both_onlyR"
   mkF "testDir/dir2/both/" "both.txt" "both"
   mkD "testDir/dir2/onlyR"
   mkD "testDir/dir2/onlyR/r1"
   mkD "testDir/dir2/onlyR/r2"
   mkD "testDir/dir2/onlyR/r2/r3"
   mkF "testDir/dir2/onlyR/r2/r3" "rtxt.txt" "rtxt"

ft1 :: Tr.Forest FilePath
ft1 = [Tr.Node ("both")
         [Tr.Node ("both1")
            [Tr.Node ("both2.txt") []],
          Tr.Node ("both_onlyL")
             [Tr.Node ("both_onlyL.txt") []],
          Tr.Node ("both.txt") []],
       Tr.Node ("onlyL")
          [Tr.Node ("l1")
             [Tr.Node ("l3")
                [Tr.Node ("l4")
                   [Tr.Node ("l2txt.txt") []]],
              Tr.Node ("ltxt.txt") []],
           Tr.Node ("l2") []]]

dt1 :: Tr.Forest (TreeDiff, FilePath)
dt1 = [Tr.Node (Both, "both")
         [Tr.Node (Both, "both1")
            [Tr.Node (Both, "both2.txt") []],
          Tr.Node (LeftOnly, "both_onlyL")
             [Tr.Node (LeftOnly, "both_onlyL.txt") []],
          Tr.Node (RightOnly, "both_onlyR")
             [Tr.Node (RightOnly, "both_onlyR.txt") []],
          Tr.Node (Both, "both.txt") []],
       Tr.Node (LeftOnly, "onlyL")
          [Tr.Node (LeftOnly, "l1")
             [Tr.Node (LeftOnly, "l3")
                [Tr.Node (LeftOnly, "l4")
                   [Tr.Node (LeftOnly, "l2txt.txt") []]],
              Tr.Node (LeftOnly, "ltxt.txt") []],
           Tr.Node (LeftOnly, "l2") []],
       Tr.Node (RightOnly, "onlyR")
          [Tr.Node (RightOnly, "r1") [],
           Tr.Node (RightOnly, "r2")
             [Tr.Node (RightOnly, "r3")
                [Tr.Node (RightOnly, "rtxt.txt") []]]]]

tr1 :: Tr.Tree Int
tr1 = Tr.Node 1 [l 4, l 3, l 1, l 8, Tr.Node 7 [l 1, l 9]]

tr2 :: Tr.Tree Int
tr2 = Tr.Node 1 [l 1, l 4, Tr.Node 7 [l 9, l 1], l 8, l 3]

tr3 :: Tr.Tree Int
tr3 = Tr.Node 1 [l 1, l 2, Tr.Node 5 [l 9, l 1], l 8, l 3]

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

-------------------------------------------------------------------------------

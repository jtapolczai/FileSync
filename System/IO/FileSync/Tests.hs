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
    TestLabel "createFileTree1" $ TestCase createFileTree1]

createEmptyFileTree :: Assertion
createEmptyFileTree =
   bracket' (mkD "testDir/empty")
            (rmD "testDir")
            (do t <- createFileTree (LR "testDir/empty")
                assertEqual "empty file tree" [] t)

createFileTree1 :: Assertion
createFileTree1 =
   bracket' (do mkD "testDir/dir1"
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
                mkF "testDir/dir2/onlyR/r2/r3" "rtxt.txt" "rtxt")
           (rmD "testDir")
           (do t <- createDiffTree (LR "testDir/dir1") (RR "testDir/dir2")
               putStrLn "Expected forest:"
               putStrLn . Tr.drawForest . map (fmap show) . sortForest $ dt 
               putStrLn "Actual forest:"
               putStrLn . Tr.drawForest . map (fmap show) . sortForest $ t 
               assertEqual "file tree 1" (sortForest dt) (sortForest t))
   where
      dt = [Tr.Node (Both, "both")
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

-- Helpers
-------------------------------------------------------------------------------

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

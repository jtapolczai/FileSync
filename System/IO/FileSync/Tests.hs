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
            (rmD "testDir/empty")
            (do t <- createFileTree (LR "testDir/empty")
                assertEqual "empty file tree" [] t)

createFileTree1 :: Assertion
createFileTree1 =
   bracket' (do mkD "testDir/dir1"
                mkD "testDir/both"
                mkD "testDir/both/both1"
                mkF "testDir/both/both1" "both2.txt" "both2_left"
                mkD "testDir/both/both_onlyL"
                mkF "testDir/both/both_onlyL" "both_onlyL.txt" "both_onlyL"
                mkF "testDir/both/" "both.txt" "both"
                mkD "testDir/onlyL"
                mkD "testDir/onlyL/l1"
                mkD "testDir/onlyL/l1/l3"
                mkD "testDir/onlyL/l1/l3/l4"
                mkF "testDir/onlyL/l1/l3/l4" "l2txt.txt" "l2"
                mkF "testDir/onlyL/l1" "ltxt.txt" "ltxt"
                mkD "testDir/onlyL/l2"

                mkD "testDir/dir2"
                mkD "testDir/both"
                mkD "testDir/both/both1"
                mkF "testDir/both/both1" "both2.txt" "both2_right"
                mkD "testDir/both/both_onlyR"
                mkF "testDir/both/both_onlyR" "both_onlyR.txt" "both_onlyR"
                mkF "testDir/both/" "both.txt" "both"
                mkD "testDir/onlyR"
                mkD "testDir/onlyR/r1"
                mkD "testDir/onlyR/r2"
                mkD "testDir/onlyR/r2/r3"
                mkF "testDir/onlyR/r2/r3" "rtxt.txt" "rtxt")
           (do rmD "testDir/dir1"
               rmD "testDir/dir2")
           (do t <- createDiffTree (LR "testDir/dir1") (RR "testDir/dir2")
               assertEqual "file tree 1" dt t)
   where
      dt = [Tr.Node (Both, "both") [],
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

module System.IO.FileSync.Tests.Tree where

import Control.Monad.Trans.Either
import Control.Monad.Trans.Tree
import qualified Data.Tree as Tr
import Test.HUnit

import System.IO.FileSync.Join
import System.IO.FileSync.Sync
import System.IO.FileSync.Tests.Tests
import System.IO.FileSync.Types

tests = TestLabel "treeTests" $ TestList
   [TestLabel "createEmptyFileTree" $ TestCase createEmptyFileTree,
    TestLabel "createFileTree1" $ TestCase createFileTree1,
    TestLabel "createFileTree2" $ TestCase createFileTree2,
    TestLabel "createEmptyDiffTree" $ TestCase createEmptyDiffTree,
    TestLabel "createDiffTree1" $ TestCase createDiffTree1,
    TestLabel "createDiffTree2" $ TestCase createDiffTree2,
    TestLabel "sortTree1" $ TestCase sortTree1,
    TestLabel "sortTree2" $ TestCase sortTree2,
    TestLabel "sortTree3" $ TestCase sortTree3,
    TestLabel "sortTree4" $ TestCase sortTree4,
    TestLabel "sortTree5" $ TestCase sortTree5,
    TestLabel "sortTree6" $ TestCase sortTree6,
    TestLabel "sortTree7" $ TestCase sortTree7,
    TestLabel "filterTree1" $ TestCase filterTree1,
    TestLabel "filterTree2" $ TestCase filterTree2,
    TestLabel "filterTree3" $ TestCase filterTree3
    ]

-- Create file tree
-------------------------------------------------------------------------------

createEmptyFileTree :: Assertion
createEmptyFileTree = bracket'
   (mkD "testDir/empty")
   (rmD "testDir")
   (do t <- createFileTree (LR "testDir/empty") >>= mapM materialize
       assertEqual "empty file tree" [] t)

createFileTree1 :: Assertion
createFileTree1 = bracket'
   testDirsL
   (rmD "testDir")
   (do t <- createFileTree lr >>= mapM materialize
       -- putStrLn "Expected forest:"
       -- putStrLn . Tr.drawForest . map (fmap show) . sortForest $ ft1
       -- putStrLn "Actual forest:"
       -- putStrLn . Tr.drawForest . map (fmap show) . sortForest $ t
       assertEqual "file tree 1" (sortForest ft1) (sortForest t))

createFileTree2 :: Assertion
createFileTree2 = bracket'
   testDirsL
   (rmD "testDir")
   (do t <- createFileTree (LR "testDir/dir1/both/both.txt") >>= mapM materialize
       assertEqual "file tree 2" t [Tr.Node (FTD "both.txt" File) []])

-- Create diff tree
-------------------------------------------------------------------------------

createEmptyDiffTree :: Assertion
createEmptyDiffTree = bracket'
   (mkD "testDir/empty")
   (rmD "testDir")
   (do (Right t) <- runEitherT $ createDiffTree (LR "testDir/empty") (RR "testDir/empty")
       assertEqual "empty file tree" [] t)

createDiffTree1 :: Assertion
createDiffTree1 = bracket'
   (testDirsL >> testDirsR)
   (rmD "testDir")
   (do (Right t) <- runEitherT $ createDiffTree lr rr
       -- putStrLn "Expected forest:"
       -- putStrLn . Tr.drawForest . map (fmap show) . sortForest $ dt1
       -- putStrLn "Actual forest:"
       -- putStrLn . Tr.drawForest . map (fmap show) . sortForest $ t
       assertEqual "diff tree 1" (sortForest dt1) (sortForest t))

createDiffTree2 :: Assertion
createDiffTree2 = bracket'
   (testDirsL >> testDirsR)
   (rmD "testDir")
   (assertEqual "duplicate-containing forests"
                (sortForest $ genericJoin [tr4] [tr5])
                (sortForest [tr6]))
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

-- Filter
-------------------------------------------------------------------------------

filterTree1 :: Assertion
filterTree1 = assertEqual "empty forest" (filterForest (>1) []) []

filterTree2 :: Assertion
filterTree2 = assertEqual "all keys >3 filtered out" (filterForest (<=3) [tr1]) [tr1']
   where
      tr1' = Tr.Node 1 [l 3, l 1]

filterTree3 :: Assertion
filterTree3 = assertEqual "all keys >10 filtered out" (filterForest (<=10) [tr1]) [tr1]

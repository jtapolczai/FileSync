{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module System.IO.FileSync.Tests.Tests where

import Control.Exception
import Control.Monad
import Control.Monad.Trans.Either
import qualified Data.Conduit as Con
import Data.Either
import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as St
import Data.Time.Calendar
import Data.Time.Clock
import qualified Data.Tree as Tr
import qualified Data.Tree.Monadic as Mt
import System.Directory
import System.IO
import System.FilePath hiding ((</>))
 -- we overwrite </> so that it only inserts slashes (/)
 -- apparently, Windows has a problem with paths that mix / and \\.
import qualified System.FilePath as FP
import Test.Hspec
import Test.Hspec.Contrib.HUnit
import Test.HUnit

import System.IO.FileSync.IO
import System.IO.FileSync.Join
import System.IO.FileSync.JoinStrategies
import System.IO.FileSync.Sync
import System.IO.FileSync.Types

import Debug.Trace (traceM)

data TwoKey = TK String String deriving (Show, Eq, Ord)

instance LooseEq TwoKey where
   type Reduct TwoKey = String
   reduct (TK s _) = s

runTests :: IO ()
runTests = hspec . fromHUnitTest $ tests

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

tests = TestList
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
    TestLabel "filterTree3" $ TestCase filterTree3,
    TestLabel "summaryJoinL_noChange" $ TestCase summaryJoinL_noChange,
    TestLabel "summaryJoinR_noChange" $ TestCase summaryJoinR_noChange,
    TestLabel "summaryJoinO_noChange" $ TestCase summaryJoinO_noChange,
    TestLabel "summaryJoinI_noChange" $ TestCase summaryJoinI_noChange,
    TestLabel "summaryJoinL1" $ TestCase summaryJoinL1,
    TestLabel "summaryJoinR1" $ TestCase summaryJoinR1,
    TestLabel "summaryJoinO1" $ TestCase summaryJoinO1,
    TestLabel "summaryJoinI1" $ TestCase summaryJoinI1,
    TestLabel "summaryJoinI2_modTime_newer" $ TestCase summaryJoinI2_modTime_newer,
    TestLabel "summaryJoinI3_modTime_identical" $ TestCase summaryJoinI3_modTime_identical,
    TestLabel "summaryJoinI4_size_larger" $ TestCase summaryJoinI4_size_larger,
    TestLabel "summaryJoinI5_size_identical" $ TestCase summaryJoinI5_size_identical,
    TestLabel "summaryJoinI6_overwriteWithLeft" $ TestCase summaryJoinI6_overwriteWithLeft,
    TestLabel "summaryJoinI7_overwriteWithRight" $ TestCase summaryJoinI7_overwriteWithRight,
    TestLabel "performSummaryJoinL1" $ TestCase performSummaryJoinL1,
    TestLabel "performSummaryJoinR1" $ TestCase performSummaryJoinR1,
    TestLabel "performSummaryJoinI1" $ TestCase performSummaryJoinI1,
    TestLabel "performSummaryJoinO1" $ TestCase performSummaryJoinO1,
    TestLabel "getFileSize1" $ TestCase getFileSize1
    ]

-- Create file tree
-------------------------------------------------------------------------------

createEmptyFileTree :: Assertion
createEmptyFileTree = bracket'
   (mkD "testDir/empty")
   (rmD "testDir")
   (do t <- createFileTree (LR "testDir/empty") >>= mapM Mt.materialize
       assertEqual "empty file tree" [] t)

createFileTree1 :: Assertion
createFileTree1 = bracket'
   testDirsL
   (rmD "testDir")
   (do t <- createFileTree lr >>= mapM Mt.materialize
       putStrLn "Expected forest:"
       putStrLn . Tr.drawForest . map (fmap show) . sortForest $ ft1
       putStrLn "Actual forest:"
       putStrLn . Tr.drawForest . map (fmap show) . sortForest $ t
       assertEqual "file tree 1" (sortForest ft1) (sortForest t))

createFileTree2 :: Assertion
createFileTree2 = bracket'
   testDirsL
   (rmD "testDir")
   (do t <- createFileTree (LR "testDir/dir1/both/both.txt") >>= mapM Mt.materialize
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
       putStrLn "Expected forest:"
       putStrLn . Tr.drawForest . map (fmap show) . sortForest $ dt1
       putStrLn "Actual forest:"
       putStrLn . Tr.drawForest . map (fmap show) . sortForest $ t
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

-- Simple join
-------------------------------------------------------------------------------

-- Summary join
-------------------------------------------------------------------------------

summaryJoin_noChange_template :: (JoinStrategy FileAction) -> Assertion
summaryJoin_noChange_template joinStrategy = bracket'
   (testDirsL >> testDirsR)
   (rmD "testDir")
   (do ftInitL <- sortForest <$> (createFileTree lr >>= mapM Mt.materialize)
       ftInitR <- sortForest <$> (createFileTree rr >>= mapM Mt.materialize)
       res <- runEitherT $ syncDirectories joinStrategy lr rr
       assertBool "synDirectories must be successful" (isRight res)
       Con.sourceToList $ fromRight res
       ftOutL <- sortForest <$> (createFileTree lr >>= mapM Mt.materialize)
       ftOutR <- sortForest <$> (createFileTree rr >>= mapM Mt.materialize)
       assertBool "left directory didn't change without join" $ ftInitL == ftOutL
       assertBool "right directory didn't change without join" $ ftInitR == ftOutR)

summaryJoinL_noChange :: Assertion
summaryJoinL_noChange = summaryJoin_noChange_template summaryLeftJoin

summaryJoinR_noChange :: Assertion
summaryJoinR_noChange = summaryJoin_noChange_template summaryRightJoin

summaryJoinO_noChange :: Assertion
summaryJoinO_noChange = summaryJoin_noChange_template summaryOuterJoin

summaryJoinI_noChange :: Assertion
summaryJoinI_noChange = summaryJoin_noChange_template summaryInnerJoin

summaryJoin_template :: (TreeDiff -> Bool)
                     -> (JoinStrategy FileAction)
                     -> Maybe [FileAction] -- ^Expected list of generated 'FileAction's.
                     -> Maybe Assertion -- ^Additional, optional assertion at the end.
                     -> Assertion
summaryJoin_template okElems strategy expectedActions ass = bracket'
   (testDirsL >> testDirsR)
   (rmD "testDir")
   (do actions' <- runEitherT $ syncDirectories strategy lr rr
       assertBool "synDirectories must be successful" (isRight actions')
       actions <- replaceSlashes <$> (Con.sourceToList $ fromRight actions')
       maybe (return ())
             (\ea -> assertEqual "list of FileActions"
                                 (St.fromList ea)
                                 (St.fromList $ toList actions))
             expectedActions
       traceM $ show actions
       mapM (performFileAction lr rr) actions
       let dt' = map (fmap fst) $ filterForest (okElems . snd) dt1
       dirsMatch <- directoryStructureMatches "testDir/dir1" dt'
       assertBool "join performed" dirsMatch
       ft1 <- sortForest <$> (createFileTree lr >>= mapM Mt.materialize)
       ft2 <- sortForest <$> (createFileTree rr >>= mapM Mt.materialize)
       assertBool "directories match after join" $ ft1 == ft2
       fromMaybe (return ()) ass)

summaryJoinL1 :: Assertion
summaryJoinL1 = summaryJoin_template
   (flip elem [LeftOnly, Both])
   summaryLeftJoin
   (Just [Copy LeftSide "both/both_onlyL",
          Copy LeftSide "onlyL",
          Delete RightSide "both/both_onlyR",
          Delete RightSide "onlyR"])
   Nothing

summaryJoinR1 :: Assertion
summaryJoinR1 = summaryJoin_template
   (flip elem [Both, RightOnly])
   summaryRightJoin
   (Just [Delete LeftSide "both/both_onlyL",
          Delete LeftSide "onlyL",
          Copy RightSide "both/both_onlyR",
          Copy RightSide "onlyR"])
   Nothing

summaryJoinO1 :: Assertion
summaryJoinO1 = summaryJoin_template
   (flip elem [LeftOnly, Both, RightOnly])
   summaryOuterJoin
   (Just [Copy LeftSide "both/both_onlyL",
          Copy LeftSide "onlyL",
          Copy RightSide "both/both_onlyR",
          Copy RightSide "onlyR"])
   Nothing

summaryJoinI1 :: Assertion
summaryJoinI1 = summaryJoin_template
   (flip elem [Both])
   summaryInnerJoin
   (Just [Delete LeftSide "both/both_onlyL",
          Delete LeftSide "onlyL",
          Delete RightSide "both/both_onlyR",
          Delete RightSide "onlyR"])
   Nothing

summaryJoinI2_modTime_newer :: Assertion
summaryJoinI2_modTime_newer = summaryJoin_template
   (flip elem [Both])
   (summaryJoin (return . Just . Delete LeftSide)
                (return . Just . Delete RightSide)
                (overwriteWithNewer lr rr))
   (Just [Delete LeftSide "both/both_onlyL",
          Delete LeftSide "onlyL",
          Delete RightSide "both/both_onlyR",
          Delete RightSide "onlyR",
          Copy RightSide "both/both1/both2.txt",
          Copy LeftSide "both/both1/both3.txt"])
   (Just (--the right "both2" and the left "both3" are newer
          assertContent "both2_right" "testDir/dir1/both/both1/both2.txt"
          >> assertContent "both2_right" "testDir/dir2/both/both1/both2.txt"

          >> assertContent "both3_left" "testDir/dir1/both/both1/both3.txt"
          >> assertContent "both3_left" "testDir/dir2/both/both1/both3.txt"))

summaryJoinI3_modTime_identical :: Assertion
summaryJoinI3_modTime_identical = summaryJoin_template
   (flip elem [Both])
   (summaryJoin (return . Just . Delete LeftSide)
                (return . Just . Delete RightSide)
                (overwriteWithNewer lr rr))
   Nothing
   (Just (--"both" should remain unchanged in both branches
          assertContent "bothL" "testDir/dir1/both/both.txt"
          >> assertContent "bothR" "testDir/dir2/both/both.txt"))

summaryJoinI4_size_larger :: Assertion
summaryJoinI4_size_larger = summaryJoin_template
   (flip elem [Both])
   (summaryJoin (return . Just . Delete LeftSide)
                (return . Just . Delete RightSide)
                (overwriteWithLarger lr rr))
   Nothing
   (Just (--the left "both2" and the right "both3" are larger
          assertContent "both2_left larger" "testDir/dir1/both/both1/both2.txt"
          >> assertContent "both2_left larger" "testDir/dir2/both/both1/both2.txt"
          >> assertContent "both3_right larger" "testDir/dir1/both/both1/both3.txt"
          >> assertContent "both3_right larger" "testDir/dir2/both/both1/both3.txt"))

summaryJoinI5_size_identical :: Assertion
summaryJoinI5_size_identical = summaryJoin_template
   (flip elem [Both])
   (summaryJoin (return . Just . Delete LeftSide)
                (return . Just . Delete RightSide)
                (overwriteWithLarger lr rr))
   Nothing
   (Just (--"both" should remain unchanged in both branches
          assertContent "bothL" "testDir/dir1/both/both.txt"
          >> assertContent "bothR" "testDir/dir2/both/both.txt"))

summaryJoinI6_overwriteWithLeft :: Assertion
summaryJoinI6_overwriteWithLeft = summaryJoin_template
   (flip elem [Both])
   (summaryJoin (return . Just . Delete LeftSide)
                (return . Just . Delete RightSide)
                (overwriteWithLeft lr))
   Nothing
   (Just (--the left version should be present in both branches
          assertContent "bothL" "testDir/dir1/both/both.txt"
          >> assertContent "bothL" "testDir/dir2/both/both.txt"
          >> assertContent "both2_left larger" "testDir/dir2/both/both1/both2.txt"
          >> assertContent "both2_left larger" "testDir/dir2/both/both1/both2.txt"
          >> assertContent "both3_left" "testDir/dir1/both/both1/both3.txt"
          >> assertContent "both3_left" "testDir/dir2/both/both1/both3.txt"))

summaryJoinI7_overwriteWithRight :: Assertion
summaryJoinI7_overwriteWithRight = summaryJoin_template
   (flip elem [Both])
   (summaryJoin (return . Just . Delete LeftSide)
                (return . Just . Delete RightSide)
                (overwriteWithRight rr))
   Nothing
   (Just (--the left version should be present in both branches
          assertContent "bothR" "testDir/dir1/both/both.txt"
          >> assertContent "bothR" "testDir/dir2/both/both.txt"
          >> assertContent "both2_right" "testDir/dir2/both/both1/both2.txt"
          >> assertContent "both2_right" "testDir/dir2/both/both1/both2.txt"
          >> assertContent "both3_right larger" "testDir/dir1/both/both1/both3.txt"
          >> assertContent "both3_right larger" "testDir/dir2/both/both1/both3.txt"))

-- Perform summary join
-------------------------------------------------------------------------------

performSummaryJoin_template
   :: (TreeDiff -> Bool)
   -> (JoinStrategy FileAction)
   -> Maybe Assertion -- ^Additional, optional assertion at the end.
   -> Assertion
performSummaryJoin_template okElems strategy ass = bracket'
   (testDirsL >> testDirsR)
   (rmD "testDir")
   (do res <- runEitherT $ syncDirectories strategy lr rr
       assertBool "synDirectories must be successful" (isRight res)
       fromRight res Con.$$ performSummaryJoin lr rr
       let dt' = map (fmap fst) $ filterForest (okElems . snd) dt1
       dirsMatch <- directoryStructureMatches "testDir/dir1" dt'
       assertBool "join performed" dirsMatch
       ft1 <- sortForest <$> (createFileTree lr >>= mapM Mt.materialize)
       ft2 <- sortForest <$> (createFileTree rr >>= mapM Mt.materialize)
       assertBool "directories match after join" $ ft1 == ft2
       fromMaybe (return ()) ass)

performSummaryJoinL1 :: Assertion
performSummaryJoinL1 = performSummaryJoin_template
   (flip elem [LeftOnly, Both])
   summaryLeftJoin
   Nothing

performSummaryJoinR1 :: Assertion
performSummaryJoinR1 = performSummaryJoin_template
   (flip elem [Both, RightOnly])
   summaryRightJoin
   Nothing

performSummaryJoinI1 :: Assertion
performSummaryJoinI1 = performSummaryJoin_template
   (flip elem [Both])
   summaryInnerJoin
   Nothing

performSummaryJoinO1 :: Assertion
performSummaryJoinO1 = performSummaryJoin_template
   (flip elem [LeftOnly, Both, RightOnly])
   summaryOuterJoin
   Nothing

-- Get file size
-------------------------------------------------------------------------------

getFileSize1 :: Assertion
getFileSize1 = bracket'
   testDirsL
   (rmD "testDir")
   (do fs1 <- getFileSize "testDir/dir1/both/both.txt"
       assertEqual "both.txt file size" 5 fs1
       fs2 <- getFileSize "testDir/dir1/both/both_onlyL/both_onlyL.txt"
       assertEqual "both_onlyL.txt file size" 10 fs2)

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
      traceM ("Getting directory contents of: " ++ fp)
      contents <- filter (not . flip elem [".",".."]) <$> getDirectoryContents fp
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

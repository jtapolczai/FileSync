module System.IO.FileSync.Tests.Join where

import Control.Monad.Trans.Either
import Control.Monad.Trans.Tree
import qualified Data.Conduit as Con
import Data.Either
import Data.Foldable (toList)
import Data.Maybe
import qualified Data.Set as St
import Test.HUnit

import System.IO.FileSync.IO
import System.IO.FileSync.Join
import System.IO.FileSync.JoinStrategies
import System.IO.FileSync.Sync
import System.IO.FileSync.Tests.Tests
import System.IO.FileSync.Types

import Debug.Trace (traceM)

tests :: Test
tests = TestLabel "joinTests" $ TestList
   [TestLabel "summaryJoinL_noChange" $ TestCase summaryJoinL_noChange,
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
    TestLabel "getFileSize1" $ TestCase getFileSize1,
    TestLabel "filterForest1" $ TestCase filterForest1
    ]

-- Summary join
-------------------------------------------------------------------------------

summaryJoin_noChange_template :: (JoinStrategy FileAction) -> Assertion
summaryJoin_noChange_template joinStrategy = bracket'
   (testDirsL >> testDirsR)
   (rmD "testDir")
   (do ftInitL <- sortForest <$> (createFileTree lr >>= mapM materialize)
       ftInitR <- sortForest <$> (createFileTree rr >>= mapM materialize)
       res <- runEitherT $ syncDirectories joinStrategy lr rr
       assertBool "synDirectories must be successful" (isRight res)
       Con.sourceToList $ fromRight res
       ftOutL <- sortForest <$> (createFileTree lr >>= mapM materialize)
       ftOutR <- sortForest <$> (createFileTree rr >>= mapM materialize)
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
       mapM_ (performFileAction lr rr) actions
       let dt' = map (fmap fst) $ filterForest (okElems . snd) dt1
       dirsMatch <- directoryStructureMatches "testDir/dir1" dt'
       assertBool "join performed" dirsMatch
       ft1 <- sortForest <$> (createFileTree lr >>= mapM materialize)
       ft2 <- sortForest <$> (createFileTree rr >>= mapM materialize)
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
       ft1 <- sortForest <$> (createFileTree lr >>= mapM materialize)
       ft2 <- sortForest <$> (createFileTree rr >>= mapM materialize)
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

-- Filter forest
-------------------------------------------------------------------------------

filterForest1 :: Assertion
filterForest1 = bracket'
   (testDirsL)
   (rmD "testDir")
   (do res <- runEitherT $ syncDirectories strategy lr rr
       assertBool "synDirectories must be successful" (isRight res)
       fromRight res Con.$$ performSummaryJoin lr rr
       let dt' = map (fmap fst) $ filterForest (okElems . snd) dt1
       dirsMatch <- directoryStructureMatches "testDir/dir1" dt'
       assertBool "join performed" dirsMatch
       ft1 <- sortForest <$> (createFileTree lr >>= mapM materialize)
       ft2 <- sortForest <$> (createFileTree rr >>= mapM materialize)
       assertBool "directories match after join" $ ft1 == ft2)

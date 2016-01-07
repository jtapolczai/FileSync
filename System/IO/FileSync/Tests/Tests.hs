module System.IO.FileSync.Tests.Tests where

import Control.Exception
import Control.Monad
import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Sequence as S
import qualified Data.Set as St
import Data.Time.Calendar
import Data.Time.Clock
import qualified Data.Tree as Tr
import System.Directory
import System.IO
import System.FilePath
import Test.Hspec
import Test.Hspec.Contrib.HUnit
import Test.HUnit

import System.IO.FileSync.Join
import System.IO.FileSync.JoinStrategies
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
    TestLabel "sortTree7" $ TestCase sortTree7,
    TestLabel "filterTree1" $ TestCase filterTree1,
    TestLabel "filterTree2" $ TestCase filterTree2,
    TestLabel "filterTree3" $ TestCase filterTree3,
    TestLabel "simpleJoinL1" $ TestCase simpleJoinL1,
    TestLabel "simpleJoinR1" $ TestCase simpleJoinR1,
    TestLabel "simpleJoinO1" $ TestCase simpleJoinO1,
    TestLabel "simpleJoinI1" $ TestCase simpleJoinI1,
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
    TestLabel "summaryJoinI5_size_identical" $ TestCase summaryJoinI5_size_identical
    ]

-- Create file tree
-------------------------------------------------------------------------------

createEmptyFileTree :: Assertion
createEmptyFileTree = bracket'
   (mkD "testDir/empty")
   (rmD "testDir")
   (do t <- createFileTree (LR "testDir/empty")
       assertEqual "empty file tree" [] t)

createFileTree1 :: Assertion
createFileTree1 = bracket'
   testDirsL
   (rmD "testDir")
   (do t <- createFileTree lr
       putStrLn "Expected forest:"
       putStrLn . Tr.drawForest . map (fmap show) . sortForest $ ft1 
       putStrLn "Actual forest:"
       putStrLn . Tr.drawForest . map (fmap show) . sortForest $ t 
       assertEqual "file tree 1" (sortForest ft1) (sortForest t))

-- Create diff tree
-------------------------------------------------------------------------------

createEmptyDiffTree :: Assertion
createEmptyDiffTree = bracket'
   (mkD "testDir/empty")
   (rmD "testDir")
   (do t <- createDiffTree (LR "testDir/empty") (RR "testDir/empty")
       assertEqual "empty file tree" [] t)

createDiffTree1 :: Assertion
createDiffTree1 = bracket'
   (testDirsL >> testDirsR)
   (rmD "testDir")
   (do t <- createDiffTree lr rr
       putStrLn "Expected forest:"
       putStrLn . Tr.drawForest . map (fmap show) . sortForest $ dt1
       putStrLn "Actual forest:"
       putStrLn . Tr.drawForest . map (fmap show) . sortForest $ t 
       assertEqual "diff tree 1" (sortForest dt1) (sortForest t))

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

simpleJoin_template :: (TreeDiff -> Bool)
                    -> JoinStrategy ()
                    -> Assertion
simpleJoin_template okElems strategy = bracket'
   (testDirsL >> testDirsR)
   (rmD "testDir")
   (do syncDirectories strategy lr rr
       let dt' = map (fmap snd) $ filterForest (okElems . fst) dt1
       dirsMatch <- directoryStructureMatches "testDir/dir1" dt'
       assertBool "join performed" dirsMatch
       ft1 <- sortForest <$> createFileTree lr
       ft2 <- sortForest <$> createFileTree rr
       assertBool "directories match after join" $ ft1 == ft2)

simpleJoinL1 :: Assertion
simpleJoinL1 = simpleJoin_template (flip elem [LeftOnly, Both]) simpleLeftJoin

simpleJoinR1 :: Assertion
simpleJoinR1 = simpleJoin_template (flip elem [Both, RightOnly]) simpleRightJoin

simpleJoinO1 :: Assertion
simpleJoinO1 = simpleJoin_template (flip elem [LeftOnly, Both, RightOnly]) simpleOuterJoin

simpleJoinI1 :: Assertion
simpleJoinI1 = simpleJoin_template (flip elem [Both]) simpleInnerJoin

-- Summary join
-------------------------------------------------------------------------------

summaryJoin_noChange_template :: (JoinStrategy (S.Seq FileAction))
                      -> Assertion
summaryJoin_noChange_template joinStrategy = bracket'
   (testDirsL >> testDirsR)
   (rmD "testDir")
   (do ftInitL <- sortForest <$> createFileTree lr
       ftInitR <- sortForest <$> createFileTree rr
       syncDirectories joinStrategy lr rr
       ftOutL <- sortForest <$> createFileTree lr
       ftOutR <- sortForest <$> createFileTree rr
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
                     -> (JoinStrategy (S.Seq FileAction))
                     -> Maybe [FileAction] -- ^Expected list of generated 'FileAction's.
                     -> Maybe Assertion -- ^Additional, optional assertion at the end.
                     -> Assertion
summaryJoin_template okElems strategy expectedActions ass = bracket'
   (testDirsL >> testDirsR)
   (rmD "testDir")
   (do actions <- syncDirectories strategy lr rr
       maybe (return ())
             (\ea -> assertEqual "list of FileActions" 
                                 (St.fromList ea) 
                                 (St.fromList $ toList actions))
             expectedActions
       mapM (performFileAction lr rr) actions
       let dt' = map (fmap snd) $ filterForest (okElems . fst) dt1
       dirsMatch <- directoryStructureMatches "testDir/dir1" dt'
       assertBool "join performed" dirsMatch
       ft1 <- sortForest <$> createFileTree lr
       ft2 <- sortForest <$> createFileTree rr
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
          putStrLn "first assert"
          >> assertContent "both2_right" "testDir/dir1/both/both1/both2.txt"
          >> putStrLn "second assert"
          >> assertContent "both2_right" "testDir/dir2/both/both1/both2.txt"

          >> putStrLn "third assert"
          >> assertContent "both3_left" "testDir/dir1/both/both1/both3.txt"
          >> putStrLn "fourth assert"
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
                (overwriteWithNewer lr rr))
   Nothing
   (Just (--the left "both2" and the right "both3" are larger
          putStrLn "first assert"
          >> assertContent "both2_left larger" "testDir/dir1/both/both1/both2.txt"
          >> putStrLn "second assert"
          >> assertContent "both2_left larger" "testDir/dir2/both/both1/both2.txt"
          >> putStrLn "third assert"
          >> assertContent "both3_right larger" "testDir/dir1/both/both1/both3.txt"
          >> putStrLn "fourth assert"
          >> assertContent "both3_right larger" "testDir/dir2/both/both1/both3.txt"))

summaryJoinI5_size_identical :: Assertion
summaryJoinI5_size_identical = summaryJoin_template
   (flip elem [Both])
   (summaryJoin (return . Just . Delete LeftSide)
                (return . Just . Delete RightSide)
                (overwriteWithNewer lr rr))
   Nothing
   (Just (--"both" should remain unchanged in both branches
          putStrLn "first assert"
          >> assertContent "bothL" "testDir/dir1/both/both.txt"
          >> putStrLn "second assert"
          >> assertContent "bothR" "testDir/dir2/both/both.txt"))

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

ft1 :: Tr.Forest FilePath
ft1 = [Tr.Node ("both")
         [Tr.Node ("both1")
            [Tr.Node ("both2.txt") [],
             Tr.Node ("both3.txt") []],
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
            [Tr.Node (Both, "both2.txt") [],
             Tr.Node (Both, "both3.txt") []],
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
directoryStructureMatches :: FilePath -> Tr.Forest FilePath -> IO Bool
directoryStructureMatches fp xs = do
   isFile <- doesFileExist fp
   isDir <- doesDirectoryExist fp
   if isFile then return $ null xs
   else if not isDir then return False
   else do
      contents <- filter (not . flip elem [".",".."]) <$> getDirectoryContents fp
      dirs <- filterM doesDirectoryExist contents
      files <- St.fromList <$> filterM doesFileExist contents

      let leaves = St.fromList . map Tr.rootLabel . filter (null . Tr.subForest) $ xs
          xsMap = M.fromList . map (\(Tr.Node y ys) -> (y,ys)) $ xs

          leavesMatch = files `St.isSubsetOf` leaves

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

-------------------------------------------------------------------------------

module Main where

import Test.Hspec
import Test.Hspec.Contrib.HUnit
import Test.HUnit

-- (1) Import your test module qualified here.
import qualified System.IO.FileSync.Tests.Join as Join
import qualified System.IO.FileSync.Tests.Tree as Tree
import qualified System.IO.FileSync.Tests.TreeT as TreeT

-- (2) Insert its test list qualified here.
main :: IO ()
main = runTests [Join.tests,
                 Tree.tests,
                 TreeT.tests]

runTests :: [Test] -> IO ()
runTests ts = hspec . fromHUnitTest . TestList $ ts

module Main where

import Test.Hspec
import Test.Hspec.Contrib.HUnit
import Test.HUnit

-- (1) Import your test module qualified here.
import qualified System.IO.FileSync.Tests.Tests as T1
import qualified System.IO.FileSync.Tests.TreeT as T2

-- (2) Insert its testlist qualified here.
main :: IO ()
main = runTests [T1.tests,
                 T2.tests]

runTests :: [Test] -> IO ()
runTests ts = hspec . fromHUnitTest . TestList $ ts

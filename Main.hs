module Main where

import qualified Data.Tree as T

import Debug.Trace

import System.IO.FileSync.Types
import System.IO.FileSync.Join
import System.IO.FileSync.Sync
import System.IO.FileSync.JoinStrategies

main :: IO ()
main = return ()

t1 = T.Node 1 []
t2 = T.Node 2 []
t3 = T.Node 3 []

t4 = T.Node 5 [t1, t2]
t5 = T.Node 5 [t2, t3]

t6 = T.Node 5 [T.Node 1 [T.Node 3 [T.Node 4 []]], T.Node 2 []]
t7 = T.Node 5 [T.Node 1 [], T.Node 2 [T.Node 6 [T.Node 7 []]]]

trace' x = traceShow x $ x

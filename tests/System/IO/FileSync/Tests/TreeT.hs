module System.IO.FileSync.Tests.TreeT where

import Control.Monad.Trans.Tree
import qualified Data.Tree as Tr
import Test.Hspec
import Test.Hspec.Contrib.HUnit
import Test.HUnit

tests = TestList
   [TestLabel "treeTEqual" $ TestCase treeTEqual,
    TestLabel "functorTreeTEqual" $ TestCase functorTreeTEqual,
    TestLabel "apTreeTEqual" $ TestCase apTreeTEqual,
    TestLabel "monadTreeTEqual1" $ TestCase monadTreeTEqual1,
    TestLabel "monadTreeTEqual2" $ TestCase monadTreeTEqual2,
    TestLabel "foldableTreeTEqual" $ TestCase foldableTreeTEqual,
    TestLabel "traversableTreeTEqual" $ TestCase traversableTreeTEqual
    ]

-- TreeT
-------------------------------------------------------------------------------

treeTEqual :: Assertion
treeTEqual = do
   let w :: Show a => a -> [b] -> IO (a,[b])
       w x xs = print x >> return (x,xs)

       t1 = TreeT (w 2
                   [TreeT (w 1 []),
                    TreeT (w 3 [])])

   putStrLn "-------------"
   t1' <- materialize t1
   putStrLn "-------------"
   t2' <- materialize t1
   assertEqual "left and right TreeT" t1' t2'

functorTreeTEqual :: Assertion
functorTreeTEqual = do
   let w :: Show a => a -> [b] -> IO (a,[b])
       w x xs = print x >> return (x,xs)

       t1 = TreeT (w 2
                   [TreeT (w 1 []),
                    TreeT (w 3 [])])
       t2 = TreeT (w 3
                   [TreeT (w 2 []),
                    TreeT (w 4 [])])

   putStrLn "-------------"
   t1' <- materialize t2
   putStrLn "-------------"
   t2' <- materialize (fmap (+1) t1)
   assertEqual "left and right TreeT" t1' t2'

apTreeTEqual :: Assertion
apTreeTEqual = do
   let w :: Show a => a -> [b] -> IO (a,[b])
       w x xs = print x >> return (x,xs)

       t1 = TreeT (w 2
                   [TreeT (w 1 []),
                    TreeT (w 3 [])])
       t2 = TreeT (w 10
                   [TreeT (w 5 []),
                    TreeT (w 15 [])])
       t3 = TreeT (w 12
                   [TreeT (w 7 []),
                    TreeT (w 17 []),
                    TreeT (w 11
                           [TreeT (w 6 []),
                            TreeT (w 16 [])
                           ]),
                    TreeT (w 13
                           [TreeT (w 8 []),
                            TreeT (w 18 [])
                           ])
                    ])

   putStrLn "-------------"
   t1' <- materialize $ (+) <$> t1 <*> t2
   putStrLn "-------------"
   t2' <- materialize t3
   assertEqual "left and right TreeT" t1' t2'


monadTreeTEqual1 :: Assertion
monadTreeTEqual1 = do
   let w :: Show a => a -> [b] -> IO (a,[b])
       w x xs = print x >> return (x,xs)

       t1 = TreeT (w 2
                   [TreeT (w 1 []),
                    TreeT (w 3 [])])
       t2 = TreeT (w 10
                   [TreeT (w 5 []),
                    TreeT (w 15 [])])

   putStrLn "-------------"
   t1' <- materialize $ t1 >>= (\x -> TreeT (w (x*5) []))
   putStrLn "-------------"
   t2' <- materialize t2
   assertEqual "left and right TreeT" t1' t2'

monadTreeTEqual2 :: Assertion
monadTreeTEqual2 = do
   let w :: Show a => a -> [b] -> IO (a,[b])
       w x xs = print x >> return (x,xs)

       t1 = TreeT (w 2
                   [TreeT (w 1 []),
                    TreeT (w 3 [])])
       t2 = TreeT (w 10
                   [TreeT (w 6 []),
                    TreeT (w 14 []),
                    TreeT (w 5
                           [TreeT (w 3 []),
                           TreeT (w 7 [])]),
                    TreeT (w 15
                           [TreeT (w 9 []),
                            TreeT (w 21 [])
                           ])
                    ])

   putStrLn "-------------"
   t1' <- materialize $ t1 >>= (\x -> TreeT (w (x*5) [TreeT (w (x*3) []),
                                                      TreeT (w (x*7) [])]))
   putStrLn "-------------"
   t2' <- materialize t2
   assertEqual "left and right TreeT" t1' t2'

foldableTreeTEqual :: Assertion
foldableTreeTEqual = do
   let w x xs = Just (x,xs)

       t1 = TreeT (w 10
                   [TreeT (w 6 []),
                    TreeT (w 14 []),
                    TreeT (w 5
                           [TreeT (w 3 []),
                           TreeT (w 7 [])]),
                    TreeT (w 15
                           [TreeT (w 9 []),
                            TreeT (w 21 [])
                           ])
                    ])

       res = [10,6,14,5,3,7,15,9,21]

   putStrLn "-------------"
   let t1' = foldMap (:[]) t1
   assertEqual "TreeT nodes in preorder" t1' res

traversableTreeTEqual :: Assertion
traversableTreeTEqual = do
   let w x xs = Just (x,xs)

       t1 = TreeT (w 10
                   [TreeT (w 6 []),
                    TreeT (w 14 []),
                    TreeT (w 5
                           [TreeT (w 3 []),
                           TreeT (w 7 [])]),
                    TreeT (w 15
                           [TreeT (w 9 []),
                            TreeT (w 21 [])
                           ])
                    ])

       t1' :: TreeT Maybe [Int]
       t1' = fmap (:[99]) t1

   putStrLn "-------------"
   let t2 :: TreeT Maybe [Int]
       t2 = traverse (\a c -> a:c) t1 [99]
       (Just t_res_1) :: Maybe (Tr.Tree [Int]) = materialize t1'
       (Just t_res_2) = materialize t2
   assertEqual "TreeT nodes in preorder" t_res_1 t_res_2

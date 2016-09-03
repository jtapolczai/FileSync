module Control.Monad.Trans.Tree (
   TreeT(..),
   nodeLabel,
   subForest,
   mapTreeT,
   -- * Running trees
   --   These functions traverse TreeTs and return pure rose trees.
   --   Their drawback is that they keep the entire tree in memory.
   --   If you are only interested in the monadic effects and not
   --   the actual values, it is best to discard the result to save space.
   materialize,
   materializePar,
   -- * Folding, unfolding TreeTs
   unfoldTreeT,
   leaves,
   justLeaves,
   traverseM,
   ) where

import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Monoid ((<>))
import qualified Data.Tree as T

import System.IO.FileSync.STM.Utils

-- |Parametrizable Tree monad, with an inner monad.
data TreeT m n = -- |An internal nodes with a value and children
                 --  wrapped in a monad.
                 TreeT (m ( n, [TreeT m n] ) )

instance (Functor m) => Functor (TreeT m) where
   fmap f (TreeT m) = TreeT $ fmap (\(x,xs) -> (f x, map (fmap f) xs)) m

instance Applicative m => Applicative (TreeT m) where
   pure x = TreeT $ pure (x, [])
   (TreeT f_arg) <*> (TreeT m_arg) =
      TreeT $ mk <$> f_arg <*> m_arg
         where
            mk (f,fs) (m,ms) = (f m, ch1 ++ ch2)
               where
                  ch1 = map (fmap f) ms
                  ch2 = map (<*> TreeT (pure (m,ms))) fs

instance Monad m => Monad (TreeT m) where
   (TreeT m_arg) >>= f = TreeT $ do
                            (m,ms) <- m_arg
                            let (TreeT x_arg) = f m
                            (x,xs) <- x_arg
                            let xs' = xs ++ map (>>= f) ms
                            pure (x, xs')

-- |Preserves the effects and lifts the computation into a one-node tree.
instance MonadTrans TreeT where
   lift m = TreeT $ (,[]) <$> m

-- |Preserves the effects and lifts the computation into a one-node tree.
instance MonadIO m => MonadIO (TreeT m) where
   liftIO m = TreeT $ (,[]) <$> (liftIO m)

instance Foldable f => Foldable (TreeT f) where
   foldMap f (TreeT m) = foldMap (\(n,ns) -> f n <> foldMap (foldMap f) ns) m

-- |Gets the node label of an TreeT.
nodeLabel :: Functor m => TreeT m n -> m n
nodeLabel (TreeT m) = fmap fst m

-- |Gets the children of an TreeT.
subForest :: Functor m => TreeT m n -> m [TreeT m n]
subForest (TreeT m) = fmap snd m

-- |Maps every element of a structure to a monadic value and collects
--  the results while preserving the original structure.
mapTreeT :: Monad m => (a -> m b) -> TreeT m a -> TreeT m b
mapTreeT f (TreeT m) = TreeT $ do
   (x,xs) <- m
   (, map (mapTreeT f) xs) <$> f x

-------------------------------------------------------------------------------

-- |Completely unrolls an 'TreeT' into a 'Tree' __depth-first__,
--  evaluating all nodes.
--
--  The time is @O(n)@ where @n@ is the number of nodes.
--  The space is @O(n)@ if the result is kept and @O(d)@ if it isn't,
--  with @d@ being the maximal depth of the tree.
materialize :: Monad m => TreeT m n -> m (T.Tree n)
materialize (TreeT m) = do
   (v, children) <- m
   children' <- mapM materialize children
   return $ T.Node v children'

-- |Unrolls an 'TreeT' into a tree, executing the monadic operations in
--  parallel. 'materializePar' is generalization of 'materialize' and is
--  superior to it if the 'TreeT' contains IO-heavy
--  operations like HTTP requests.
--
--  The time @Omega(n/t)@, where @n@ is the number of nodes and @t@ is the
--  size of the thread pool - with optimal parallelism, @t@ nodes can be
--  processed at once (although this depends on the shape of the tree. In
--  the worst case of a list, @t@ makes no difference at all.
--  The space is @O(n)@ if the result is kept and @O(dt)@ if it isn't.
--
--  Note that a node's children may be rearranged, depending
--  on the order in which their processing finishes.
materializePar :: TaskLimit
                  -- ^The upper limit on simultaneous tasks.
                  --  For @n=1@, 'materializePar' behaves identically to
                  --  materialize. For very large @n@, every node gets its own
                  --  thread. Depending on the IO operations, this value should
                  --  be kept within reason.
               -> TreeT IO n
               -> IO (T.Tree n)
materializePar numTasks (TreeT m) = do
   (v, children) <- withTaskLimit numTasks m
   results <- parMapSTM (materializePar numTasks) children
   return $ T.Node v results

-------------------------------------------------------------------------------

-- |Unfolds an 'TreeT' from a monadic value.
--  Analogous to 'Data.Tree.unfoldTreeM'
unfoldTreeT :: Monad m => (b -> m (a, [b])) -> m b -> TreeT m a
unfoldTreeT f x = TreeT $ do (y, ys) <- x >>= f
                             return $ (y, map (unfoldTreeT f . return) ys)

-- |Leaf function on trees.
leaves :: (s -> n -> a) -- ^Result calculator, applied to the leaves.
       -> (s -> n -> s) -- ^State updater, applied on non-leaves.
       -> s -- ^Initial state.
       -> T.Tree n
       -> [a]
leaves f _ seed (T.Node n []) = [f seed n]
leaves f g seed (T.Node n xs) = concatMap (leaves f g (g seed n)) xs

traverseM :: Monad m
          => (s -> n -> m (s,a))
          -> s
          -> TreeT m n
          -> TreeT m a
traverseM f st (TreeT m) = TreeT $ do
   (n, ns) <- m
   (st',n') <- f st n
   return $ (n', map (traverseM f st') ns)

-- |Collects just the leaves of a tree. Convenience function.
justLeaves :: (n -> a) -> T.Tree n -> [a]
justLeaves f = leaves (const f) undefined undefined

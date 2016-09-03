module System.IO.FileSync.STM.Utils where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TQueue
import Data.Foldable (traverse_)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Loops (whileJust)
import Control.Monad.STM

-- |Abstract type for a limit on the number of concurrent tasks.
--  This is a semaphore that might have an infinite value (no limit).
newtype TaskLimit = TaskLimit (TVar (Maybe Int))

-- |Descreases a task limit by 1, if it exists. If the value is already at
--  0, the function blocks.
addTask :: TaskLimit -> STM ()
addTask (TaskLimit v) = do
   v' <- readTVar v
   case v' of Nothing  -> return ()
              Just v'' -> if v'' <= 0 then retry
                                      else writeTVar v (fmap (subtract 1) v')

-- |Increases a value by one. A semaphore's "signal"-
removeTask :: TaskLimit -> STM ()
removeTask (TaskLimit l) = modifyTVar' l (fmap (+1))

-- |Performs an IO action, taking into account a TaskLimit, i.e.
--  blocking while it is @<=0@ and then temporarily decrementing it.
--
--  Note: this does not spawn a new thread.
withTaskLimit :: MonadIO m => TaskLimit -> m a -> m a
withTaskLimit lock m = do liftIO $ atomically $ addTask lock
                          res <- m
                          liftIO $ atomically $ removeTask lock
                          return res

-- |Reads all elements of a queue.
readWholeQueue :: TQueue a -> STM [a]
readWholeQueue q = whileJust (tryReadTQueue q) return

-- |Executes a collection of actions with a thread pool,
--  gathering the results in a qeue.
--
--  The function blocks until all actions have finished.
--  Ordering of the results is not guaranteed;
--  they are inserted as each task finishes.
--
--  This function is morally identical to 'Control.Parallel.Strategies.parMap'
--  in Control.Parallel.Strategies, with the difference that it takes
--  an @a -> IO b@ instead of @a -> b@.
parMapSTM :: (a -> IO b)
             -- ^The function which is to be applied to each element
             --  of the input.
          -> [a]
             -- ^Input data.
          -> IO [b]
parMapSTM f inp = do
   remaining <- atomically $ newTVar $ length inp
   results <- atomically newTQueue
   traverse_ (f' remaining results) inp
   atomically (readTVar remaining >>= check . (<=0))
   atomically (readWholeQueue results)
   where
      f' remaining results n =
         forkIO (do result <- f n
                    atomically (writeTQueue results result
                                >> modifyTVar remaining (subtract 1)))

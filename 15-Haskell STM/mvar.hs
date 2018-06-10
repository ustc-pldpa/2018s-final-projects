module Main where

import Control.Monad
import Control.Concurrent
import Data.IORef

data Thread = Thread (MVar ())

fork f = do
  mutex <- newEmptyMVar
  forkIO $ f >> putMVar mutex ()
  return $ Thread mutex

wait (Thread mutex) = takeMVar mutex

data Account = Account (IORef Int) (MVar ())

newAccount balance = do
  balance' <- newIORef balance
  mutex <- newMVar ()
  return $ Account balance' mutex

readAccount (Account balance _) = do
  balance' <- readIORef balance
  return balance'

depositAccount (Account balance mutex) value = withMVar mutex $ \_ -> do
  balance' <- readIORef balance
  writeIORef balance $ balance' + value

withdrawAccount (Account balance mutex) value = withMVar mutex $ \_ -> do
  balance' <- readIORef balance
  when (balance' >= value) $ do
    writeIORef balance $ balance' - value

main = do
  account <- newAccount 0
  t1 <- fork $ replicateM_ 100 $ depositAccount account 2
  t2 <- fork $ replicateM_ 100 $ withdrawAccount account 2
  pure $ wait <$> [t1, t2]
  print =<< readAccount account


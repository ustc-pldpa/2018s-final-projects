module Main where

import Control.Monad
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM

data Thread = Thread (MVar ())

fork f = do
ls  mutex <- newEmptyMVar
  forkIO $ f >> putMVar mutex ()
  return $ Thread mutex

wait (Thread mutex) = takeMVar mutex

data Account = Account (TVar Int) (TMVar ())

newAccount balance = do
  mutex <- newTMVar ()
  balance' <- newTVar balance
  return $ Account balance' mutex

readAccount (Account balance mutex) = do
  balance' <- readTVar balance
  return balance'

withdrawAccount (Account balance mutex) value = do
  takeTMVar mutex
  balance' <- readTVar balance
  check $ balance' >= value
  writeTVar balance $ balance' - value
  putTMVar mutex ()

depositAccount (Account balance mutex) value = do
  takeTMVar mutex
  balance' <- readTVar balance
  writeTVar balance $ balance' + value
  putTMVar mutex ()

transferAccount accountFrom accountTo value = do
  withdrawAccount accountFrom value
  depositAccount accountTo value

main = do
  accountFrom <- atomically $ newAccount 0
  accountTo <- atomically $ newAccount 0
  t1 <- fork $ replicateM_ 100 $ atomically $ depositAccount accountFrom 20
  t2 <- fork $ replicateM_ 100 $ atomically $ transferAccount accountFrom accountTo 10
  t3 <- fork $ replicateM_ 100 $ atomically $ transferAccount accountFrom accountTo 10
  pure $ wait <$> [t1, t2, t3]
  print =<< atomically (readAccount accountFrom)
  print =<< atomically (readAccount accountTo)



module Main where

import Control.Monad
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM

data Account = Account (TVar Int)

newAccount balance = do
  balance' <- newTVar balance
  return $ Account balance'

readAccount (Account balance) = do
  balance' <- readTVar balance
  return balance'

withdrawAccount (Account balance) value = do
  balance' <- readTVar balance
  check $ balance' >= value
  writeTVar balance $ balance' - value

depositAccount (Account balance) value = do
  balance' <- readTVar balance
  writeTVar balance $ balance' + value

transferAccount accountFrom accountTo value = do
  withdrawAccount accountFrom value
  depositAccount accountTo value

main = do
  success <- atomically $ newTVar False
  accountFrom <- atomically $ newAccount 0
  accountTo <- atomically $ newAccount 2000
  let printAccount = do
        print =<< atomically (readAccount accountFrom)
        print =<< atomically (readAccount accountTo)
        print =<< atomically (readTVar success)
  let c1 = do
        writeTVar success True
        transferAccount accountFrom accountTo 1
      c2 = do
        return ()
    in atomically $ c1 <|> c2
  printAccount
  let c1 = do
        writeTVar success True
        transferAccount accountTo accountFrom 1
      c2 = do
        return ()
    in atomically $ c1 <|> c2
  printAccount

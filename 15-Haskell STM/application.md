# Haskell中STM的应用

### 1. 概要

STM在Haskell的使用十分广泛。首先是用于处理事务，本文中会结合具体例子分析；还有就是用于构成异步运算。可以说，STM是Haskell中一个很基本的单子。

### 2. IO单子的并发及其扩展

先从IO单子说起。IO单子是更加基本的一个单子，例如Haskell中程序的入口main就属于这个类型。它实现了Functor, Applicative, Alternative, Monad, MonadPlus, MonadFail, MonadFix，单位半群Monoid，以及MonadFork。IO单子下提供`forkIO`和`forkOS`函数来实现多线程，其中前者是由GHC提供的轻量级线程，后者则是使用系统级的线程。`forkIO`调用了底层`fork#`函数，其实现如下：

```haskell
data ThreadId# :: TYPE UnliftedRep
fork# :: a -> State# RealWorld -> (#State# RealWorld, ThreadId##)

forkIO :: IO () -> IO ThreadId
forkIO action = IO $ \ s ->
   case (fork# action_plus s) of (# s1, tid #) -> (# s1, ThreadId tid #)
 where
  action_plus = catchException action childHandler
```

可以看出，这段代码实质上是在底层的`fork#`之上增加了把异常传递给`childHandler`的操作。`childHandler`则是递归地把错误甩给`realHandler`，这个真正处理异常的函数根据错误类型做出不同的选择。所以`forkIO`并不主动提供同步机制。

并行以单子为单位，所有实现了MonadFork的类型都可以被视为可并发的单子。实现MonadFork，需要提供`fork`函数，而IO类型的`fork`函数就是`forkIO`。同时，基于单子变换产生类型可以自动推导出`fork`函数，最终仍是归约到`forkIO`上。

### 3. IO单子上的变量和同步机制

IO提供了一个使用的变量数据`IORef`，它是从`STRef`化用而来，底层则是利用了`MutVar#`及其一系列操作函数。有了`IORef`，就可以在IO的演算上使用变量。从底层到IO的定义如下（省略了类型类的实现）：

```haskell
data MutVar# (a :: *) (b :: *) :: TYPE UnliftedRep

data STRef s a = STRef (MutVar# s a)

newSTRef :: a -> ST s (STRef s a)
newSTRef init = ST $ \s1# ->
    case newMutVar# init s1# of (# s2#, var# #) -> (# s2#, STRef var# #)

readSTRef :: STRef s a -> ST s a
readSTRef (STRef var#) = ST $ \s1# -> readMutVar# var# s1#

writeSTRef :: STRef s a -> a -> ST s ()
writeSTRef (STRef var#) val = ST $ \s1# ->
	case writeMutVar# var# val s1# of s2# -> (# s2#, () #)

newtype IORef a = IORef (STRef RealWorld a)

newIORef :: a -> IO (IORef a)
newIORef v = stToIO (newSTRef v) >>= \ var -> return (IORef var)

readIORef :: IORef a -> IO a
readIORef (IORef var) = stToIO (readSTRef var)

writeIORef :: IORef a -> a -> IO ()
writeIORef (IORef var) v = stToIO (writeSTRef var v)
```

除此之外，`IORef`还提供原子化操作的方式来修改，同样是利用底层的函数：

```haskell
atomicModifyIORef :: IORef a -> (a -> (a,b)) -> IO b
atomicModifyIORef (IORef (STRef r#)) f = IO $ \s -> atomicModifyMutVar# r# f s
```

尽管有原子化的操作，但不意味着可以用其来进行同步的控制，因为没有提供阻塞的方法。除此之外，其对异常的处理也不是很安全。`MVar`是定义在IO上的互斥变量，可以用之来保护互斥数据，或者进行加锁的操作。`MVar`同时提供了阻塞和非阻塞的访问方式，可以说十分的灵活。不出意外的，`MVar`来自底层`MVar#`的包装，其操作函数也是一系列底层函数的包装。

```haskell
data MVar a = MVar (MVar# RealWorld a)

readMVar :: MVar a -> IO a
readMVar (MVar mvar#) = IO $ \ s# -> readMVar# mvar# s#

takeMVar :: MVar a -> IO a
takeMVar (MVar mvar#) = IO $ \ s# -> takeMVar# mvar# s#

putMVar :: MVar a -> a -> IO ()
putMVar (MVar mvar#) x = IO $ \ s# ->
    case putMVar# mvar# x s# of s2# -> (# s2#, () #)
```

当`MVar`中的值被take走之后，视当前`MVar`中的值离开了`MVar`对象，直到再有新的值被放入。利用这样的同步特性，可以使得`forkIO`产生的线程和主线程同步。这里自定义了`Thread`类型和`fork`，`wait`函数。注意由于自定义的`Thread`不是单子，所以和MonadFork的`fork`类型冲突。

```haskell
data Thread = Thread (MVar ())

fork f = do
  mutex <- newEmptyMVar
  forkIO $ f >> putMVar mutex ()
  return $ Thread mutex

wait (Thread mutex) = takeMVar mutex
```

这样，我们就可以使用`fork`来得到新线程，然后使用`wait`来进行同步，其原理就是使用MVar在线程间传递信息。当然，这里是简单的自定义实现，忽略了异常处理的复杂性。

#### 2.1. 简单的小程序I

尽管`MVar`提供了阻塞，但是其数据的访问速度不如`IORef`。虽然，在只有金额的账户类型中，使用两者结合的形式并不会提升效率，但在接下来的一个银行账户的样例程序中，还是使用MVar来构建锁，而用`IORef`来描述变量，毕竟这样更能模拟一般的使用场景。

```haskell
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
```

### 3. STM上的变量和同步机制

加入我们为上面的模型增加`transferAccount`操作，那么就需要把`depositAccount`和`withdrawAccount`合起来成为一个事务。否则如果一边的取进行顺利而另一边的存出现异常，或者相反——都会违背我们的意图。STM通过`atomically`将一系列STM单子上的运算变为IO上的原子操作，从而描述了事务。为此，需要重新包装产生新的变量类型来应用在STM运算上。先来实现一下`IORef`的替代品`TVar`。

```haskell
newtype TVar a = TVar (IORef a)

newTVar :: a -> STM (TVar a)
newTVar a = STM $ const $ newTVarIO a

readTVar :: TVar a -> STM a
readTVar (TVar ref) = STM $ const $ readIORef ref

writeTVar :: TVar a -> a -> STM ()
writeTVar (TVar ref) a = STM $ \r -> do
    oldval <- readIORef ref
    modifyIORef r $ writeIORef ref oldval >>
    writeIORef ref a
```

同样的，借助`Maybe`类型，可以将`TVar`推广到`TMVar`。

```haskell
newtype TMVar a = TMVar (TVar (Maybe a))

newTMVar :: a -> STM (TMVar a)
newTMVar a = do
  t <- newTVar $ Just a
  return $ TMVar t

takeTMVar :: TMVar a -> STM a
takeTMVar (TMVar t) = do
  m <- readTVar t
  case m of
    Nothing -> retry
    Just a  -> do 
    	writeTVar t Nothing
        return a
    
putTMVar :: TMVar a -> a -> STM ()
putTMVar (TMVar t) a = do
  m <- readTVar t
  case m of
    Nothing -> do 
    	writeTVar t (Just a)
    	return ()
    Just _ -> retry
```

`TMVar`可视作`MVar`在STM的替代品，其通过`retry`来在内容为`Nothing`时达成阻塞。同时，我们也很容易得到非阻塞读取或写入的方法——直接根据模式匹配的结果返回`True`并执行操作或返回`False`。

#### 3.1. 简单的小程序II

同样是描述银行存取款的程序，看一下`TVar`的使用以及STM的`retry-orElse`单位半群的运行方式。

```haskell
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
  -- 0
  -- 2000
  -- False
  let c1 = do
        writeTVar success True
        transferAccount accountTo accountFrom 1
      c2 = do
        return ()
    in atomically $ c1 <|> c2
  printAccount
  -- 1
  -- 1999
  -- True
```

从运行结果可以看出，尽管c1的写操作在转账操作之前，但是一旦转账操作失败而执行另一个分支c2成功后，`success`并没有被写入。上面的程序采用互斥变量的写法则在下方，同样使用`TMVar`结合`TVar`来改写。

#### 3.2. 简单的小程序III

```haskell
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
```

 这里`TMVar`的操作和`MVar`中的一个不同是：作为锁的时候，后者推荐采用`with...`的形式，而前者没有这个必要。原因是如果将`TMVar`中的数据读走，而在之后又出现错误，也会因为事务的失败而得到数据没被读走的情况——这就不需要释放锁的操作。

### 4. 使用STM实现异步

先来说一下异步的目标：我们至少要实现两个函数，`async`和`await`。前者接受一个IO单子演算并异步地执行，而后者得到前者结果。另外，异步执行的程序可以主动或被动地放弃目前的执行资源，并在合理的时候被唤醒来完成剩下的任务。考虑到异步的异常处理比较复杂，不展开介绍`mask`等函数。

```haskell
data Async a = Async { asyncThreadId :: ThreadId, asyncWait :: STM (Either SomeException a) }

async :: IO a -> IO (Async a)
async action = do
	var <- newEmptyTMVarIO
	t <- mask $ \restore ->
		forkIO $ try (restore action) >>= atomically . putTMVar var
	return $ Async t $ readTMVar var

await :: Async a -> IO a
await a = atomically $ do
	let Async (_ a') = a in r <- a'
	either throwSTM return r
```

上面的实现简化了Control.Concurrent.Async中`Async`类型即相关函数的实现。标准库提供了很多实用的函数来构建异步操作，比如`cancel`、`race`、`concurrently`等。这些通过STM构建的操作简化了异步计算的实现方式的同时，**避免了这些函数的语法化**——就像大部分支持异步的语言所做的简化一样。在函数作为一等公民的情况下，我们就可以使用各种升格和代数操作来更简洁地完成计算。

### 参考资料

魔力Haskell https://github.com/winterland1989/magic-haskell

Real World Haskell http://book.realworldhaskell.org/

Haskell Package Documentation:

GHC.Ext http://hackage.haskell.org/package/base-4.11.1.0/docs/GHC-Exts.html

System.IO https://hackage.haskell.org/package/base-4.9.0.0/docs/System-IO.html

Control.Concurrent https://hackage.haskell.org/package/base-4.9.1.0/docs/Control-Concurrent.html

Control.Concurrent.MVar http://hackage.haskell.org/package/base-4.11.1.0/docs/Control-Concurrent-MVar.html

Control.Concurrent.STM.TVar http://hackage.haskell.org/package/stm-2.4.5.0/docs/Control-Concurrent-STM-TVar.html

Control.Concurrent.STM.TMVar http://hackage.haskell.org/package/stm-2.4.5.0/docs/Control-Concurrent-STM-TMVar.html

Control.Concurrent.Async https://hackage.haskell.org/package/async-2.2.1/docs/Control-Concurrent-Async.html

Control.Monad.Fork.Class http://hackage.haskell.org/package/monad-fork-0.1/docs/Control-Monad-Fork-Class.html
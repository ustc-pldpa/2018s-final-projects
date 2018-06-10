# Haskell中STM的抽象

### 1. 概要

为了维护语言的纯洁性，纯函数式语言在实现过程性的操作时有诸多的不便。过程性的程序中，通常会执行带有副作用的操作，然而函数的副作用时常会在Lazy Evaluation中被短路。这一点对于并发的程序则更加复杂。除此之外，纯函数式语言里相同的表达式应当具有相同的结果，这与事务的含义相抵触。尽管上述的这些麻烦给STM的抽象带来了不便，然而通过Monad机制，Haskell早已解决这些难题。因此对STM的抽象也是水到渠成的。

在应用中，我们使用Control.Monad.STM模块中的STM。该模块只负责给出抽象，而不关心编译器具体的实现。类型类相当于一个数据类型应该实现的接口，而STM类型需要实现下图的所有类型类（除了MonadFail，毕竟STM在处理错误的要求更高；这幅图需要单独提出来，是因为早期MonadFail是合并在Monad中的，此处的Monad是不必实现MonadFail的Monad）。这些类型类具有图示的继承关系，并且随着继承逐步得到抽象复杂过程的能力。接下来将依次展开每一个类型类的定义、作用和提供的函数。

请注意，本文里的函数定义仅仅是逻辑上的定义，与Haskell真正的实现并非完全一致。所有的底层标识和程序注解也同样忽略，只保留逻辑上的语义正确。

![](inheritance.png)

### 2. 函子

`Functor`通常称作函子，实现函子的f可以通过`fmap`函数从`a`的函数中抽象出`f a`的函数。更一般地，一个`a -> b`类型的函数可以被`fmap`映射成`f a -> f b`的函数。`fmap`需要满足如下的条件：

```Haskell
fmap :: (a -> b) -> f a -> f b
fmap id = id
fmap (f . g) = fmap f . fmap g
```

其中`id x = x`，`f $ x = f x `，`f . g = \x -> f $ g x`。满足这两个条件的`fmap`对`a -> b`来说是唯一的，并且在Haskell的约定下，是可以自动推导出来的。实现了`fmap`后，便获得了一系列实用的函数，其中`const x y = x`，`flip f x y = f y x`。

```Haskell
(<$>) :: Functor f => (a -> b) -> f a -> f b infixl 4
(<$>) = fmap

(<$) :: Functor f => a -> f b -> f a infixl 4
(<$) = fmap . const

($>) :: Functor f => f a -> b -> f b infixl 4
($>) = flip (<$)

(<&>) :: Functor f => f a -> (a -> b) -> f b infixl 1
(<&>) = flip fmap

void :: Functor f => f a -> f ()
void x = () <$ x
```

### 3. 应用函子

函子的局限性在于，其不支持从f中提取出函数然后应用到f的数据上。为了得到过程化的顺序执行，必须打破这个局限性。提供了apply函数`<*>`和`pure`函数的函子实现了应用函子类，需要满足同一规则、组合规则、同态规则和交换规则：

```haskell
pure id <*> v = v
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
pure f <*> pure x = pure (f x)
pure ($ y) <*> u = u <*> pure y
```

上述条件并没有限定应用函子一定需要是函子（Haskell Report 2010），不过在最新的版本中已经逐步限制从函子到应用函子，再到单子的强制继承关系。不过也可以很容易的发现，应用函子的`fmap`如果存在，则满足：

```haskell
fmap f x = pure f <*> x
```

同样，以下推导出的函数也都很常用，尤其是抽象的分支和循环。

```haskell
(*>) :: f a -> f b -> f b infixl 4
u *> v = (id <$ u) <*> v

(<*) :: f a -> f b -> f a infixl 4
(<*) = flip (*>)

(<**>) :: Applicative f => f a -> f (a -> b) -> f b infixl 4
(<**>) = flip (<*>)

when :: Applicative f => Bool -> f () -> f ()
when p s  = if p then s else pure ()

unless :: Applicative f => Bool -> f () -> f ()
unless p s = if p then pure () else s

forever :: Applicative f => f a -> f b
forever a = let a' = a *> a' in a'
```

### 4. 单子

单子Monad是使得程序顺序执行的类型类，其运算的核心是使得`Monad f => a => f b`类型的函数能够一直迭代下去的bind函数`>>=`。同时，Monad具有`return`和`ap`函数，满足如下条件：

```haskell
return k >>= m = m k
m >>= return = m
m >>= (\x -> k x >>= h) = (m >>= k) >>= h
```

从Applicative继承的`pure`应当用来描述`return`，同时继承的`<*>`能用来描述`ap`。但是由于`ap`有更一般地形式，所以通常是反过来的。由于bind函数会给右端的函数传一个参数，而对于不传递参数的情况，我们使用`*>`来表示`>>`。另外，我们还可以推导出`fmap`用bind和`return`组合的表达式，这样增强了自动推导的能力。

```haskell
return = pure
ap f x = f >>= \f' -> x >>= \x' -> return $ f' x'
<*> = ap
(>>) = (*>)
fmap f x = x >>= return . f
```

在传统的单子定义中，使用`join`而不是bind函数来作为基础。另外，还有一些常用的控制函数可以使用，其中`seq f g = g`，但如果f出现异常即bottom，则该表达式结果是bottom。

```haskell
join :: (Monad m) => m (m a) -> m a
join x = x >>= id

(=<<) :: Monad m => (a -> m b) -> m a -> m b infixr 1
(=<<) = flip (>>=)

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
f >=> g = \x -> f x >>= g

(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
(<=<) = flip (>=>)

(<$!>) :: Monad m => (a -> b) -> m a -> m b
f <$!> m = do
  x <- m
  let z = f x
  seq z return z
```

接下来，可以得到replicate和for的在单子上的抽象函数（只是等价的表达，与源码中的写法稍有区别，至少使实用中需要强调这些函数的内联性），以便于书写循环。

```haskell
replicateM :: Monad m => Int -> m a -> m [a]
replicateM 0 f = pure []
replicateM n f = fmap (:) f $ replicateM n - 1 f

replicateM_ :: Monad m => Int -> m a -> m ()
replicateM_ 0 _ = pure ()
replicateM_ n f = f *> replicateM_ (n - 1) f

mapM :: Monad m => (a -> m b) -> t a -> m (t b)
mapM = traverse

mapM_ :: Foldable t, Monad m => (a -> m b) -> t a -> m ()
mapM_ f= foldr ((>>) . f) $ return ()

forM :: Monad m => t a -> (a -> m b) -> m (t b)
forM = flip mapM

forM_ :: Foldable t, Monad m => t a -> (a -> m b) -> m ()
forM_ = flip mapM_
```

#### 4.1.  单子不动点

MonadFix是实现单子上的不动点算子的类型类，对于单子不动点算子`mfix`，需要满足purity、tightening、sliding和nesting条件下的一致性，其中单子升格函数`liftM f m = m >>= \x -> return $ f x `，普通函数的不动点算子`fix f = let x = f x in x`。

```haskell
mfix (return . h) = return (fix h)
mfix (\x -> a >>= \y -> f x y) = a >>= \y -> mfix (\x -> f x y)
mfix (liftM h . f) = liftM h (mfix (f . h))
mfix (\x -> mfix (\y -> f x y)) = mfix (\x -> f x x)
```

我们至少有一个满足上述条件的实现，虽然通常情况下其规约过程是低效的：

```haskell
mfix f = fix (>>= f)
```

#### 4.2. 可失败单子

MonadFail提供了在单子推导中，模式匹配失败的恢复策略：`fail`，它接受一个失败的`String`。值得注意的是，`fail`函数对于bind而言相当于一个左侧零元，即：

```haskell
fail s >>= f = fail s
```

举例来说，Maybe类型的`fail`函数应该这样实现：

```Haskell
data Maybe a = Nothing | Just { UnJust :: a }
fail _ = Nothing
```

### 5. 选择函子

Alternative是具有单位半群性质的应用函子，其中单位元意味着选择了做出了“不可能的选择”或者说是“失败的选择”。选择函子的使用意味着我们可以在过程失败时选择替代的过程，而不是只能接受失败，毕竟通常的应用函子进入失败选项后便无法通过应用来恢复。 选择函子需要实现单位半群的运算，在这里是or函数`<|>`，以及零元`empty`，并且满足单位半群的性质：

```haskell
empty <|> a = a <|> empty = empty
(a <|> b) <|> c = a <|> (b <|> c)
```

举例来说，Maybe类型的`<|>`函数和`empty`可以这样实现：

```haskell
empty = Nothing
Nothing <|> r = r
l <|> _ = l
```

我们可以据此写出`guard`函数，它可以过滤掉不符合要求的结果，Haskell还提供了专门的语法糖支持：

```haskell
guard :: Alternative f => Bool -> f ()
guard True = pure ()
guard False = empty
```

#### 5.1. 选择单子

MonadPlus同时实现了Alternative和Monad。由于历史原因，其需要实现`mzero`和`mplus`。从Monad的角度上，单位元需要成为bind函数的左零元，于是有：

```haskell
mzero >>= f = mzero
v >> mzero = mzero
```

默认的实现是继承自Alternative的`<|>`和`empty`，事实上笔者比起`mplus`更喜欢使用中缀的`<|>`。

```haskell
mzero = empty
mplus = (<|>)
```

### 6. 自动推导

对于用户的自定义类型，我们通常希望可以通过已有类型自动满足每个类型类的需求。Haskell自带的deriving已经可以满足很多条件下的使用，事实上，我们可以借助单子同态（通常是针对单子，但也可以用于函子等类型）在大部分情况下实现每个必要函数的推导。为此，只需提供close和open函数。通常情况下，close是构造子，而open是构造子的反函数。

```haskell
data Iso m n = Iso { close :: forall a. m a -> n a,
                     open  :: forall a. n a -> m a }

derive_fmap :: (Functor m) => Iso m n -> (a -> b) -> n a -> n b
derive_fmap iso f m = close iso (fmap f (open iso m))

derive_pure :: (Applicative m) => Iso m n -> a -> n a
derive_pure iso a = close iso (pure a)

derive_apply :: (Applicative m) => Iso m n -> n (a -> b) -> (n a -> n b)
derive_apply iso f x = close iso (open iso f <*> open iso x)

derive_empty :: (Alternative m) => Iso m n -> n a
derive_empty iso = close iso empty

derive_or :: (Alternative m) => Iso m n -> n a -> n a -> n a
derive_or iso a b = close iso (open iso a <|> open iso b)

derive_return :: (Monad m) => Iso m n -> (a -> n a)
derive_return iso a = close iso (return a)

derive_bind :: (Monad m) => Iso m n -> n a -> (a -> n b) -> n b
derive_bind iso m k = close iso ((open iso m) >>= \x -> open iso (k x))

derive_fail :: (Monad m) => Iso m n -> String -> n a
derive_fail iso a = close iso (fail a)

derive_mfix :: (MonadFix m) => Iso m n -> (a -> n a) -> n a
derive_mfix iso f = close iso (mfix (open iso . f))

derive_mzero :: MonadPlus m => Iso m n -> n a
derive_mzero iso = close iso mzero

derive_mplus :: MonadPlus m => Iso m n -> n a -> n a -> n a
derive_mplus iso n1 n2 = close iso (mplus (open iso n1) (open iso n2))
```

### 7. 软件事务内存

STM的两个关键函数是`retry`和`orElse`。`retry`表示当前事务的执行失败，需要丢弃已经执行的所有操作，然后通过`orElse`来执行恢复策略。`retry#`和`orElse#`是两个底层函数，将会在第三部分详细说明其动作。

```haskell
retry# :: State# RealWorld -> (#State# RealWorld, a#)

catchRetry# :: (State# RealWorld -> (#State# RealWorld, a#)) -> (State# RealWorld -> (#State# RealWorld, a#)) -> State# RealWorld -> (#State# RealWorld, a#)

retry :: STM a
retry = STM $ \s# -> retry# s#

orElse :: STM a -> STM a -> STM a
orElse (STM m) e = STM $ \s -> catchRetry# m (unSTM e) s
```

由于IO类型是我们通常执行顺序操作所使用的单子，所以STM提供的`atomically`操作将一系列STM操作绑定到一个IO操作中，这一组STM操作是一致的：即选择一个`orElse`分支全部执行。

```haskell
atomically# :: (State# RealWorld -> (#State# RealWorld, a#)) -> State# RealWorld -> (#State# RealWorld, a#)

atomically :: STM a -> IO a
atomically (STM m) = IO $ \s -> (atomically# m) s
```

在实践中，`check`经常用来替代retry来增强代码可读性，而`always`用来表示一定能成功地事务。一旦`always`出错而失败，`errorWithoutStackTrace`是不提供调用栈信息的。

```haskell
check :: Bool -> STM ()
check b = if b then return () else retry

check# :: (State# RealWorld -> (#State# RealWorld, a#)) -> State# RealWorld -> State# RealWorld

checkInv :: STM a -> STM ()
checkInv (STM m) = STM $ \s -> case (check# m) s of s' -> (# s', () #)

alwaysSucceeds :: STM a -> STM ()
alwaysSucceeds i = do ( i >> retry ) `orElse` ( return () ) checkInv i

always :: STM Bool -> STM ()
always i = alwaysSucceeds $ i >>= \v if v then return () else errorWithoutStackTrace
	"Transactional invariant violation"
```

这部分的最后，让我们来看看STM是如何实现每个类型类的。其中，`RealWorld`是一个底层的原生类型，用来建模Haskell的外部。在`RealWorld`中的对象可以是可变的，不过绑定依然是不可变的。除此之外，`(# #)`是不可解包的底层元组，而`State#`同样是一个原生底层类型。

```haskell
newtype STM a = STM (unSTM :: State# RealWorld -> (# State# RealWorld, a #))

instance Functor STM where
	fmap f x = x >>= (pure . f)

instance Applicative STM where
    pure x = STM (\s -> (# s, x #))
    (<*>) = ap
    (STM m) *> k = STM $ \s -> case m s of (# new_s, _ #) -> unSTM k new_s

instance Monad STM where
    return = pure
    (>>) = (*>)
    (STM m) >>= k = STM $ \s -> case m s of (# new_s, a #) -> unSTM (k a) new_s

instance Alternative STM where
	empty = retry
	<|> = orElse

instance MonadPlus STM where
	mzero = empty
	mplus = retry
```

我们不如把STM包裹的函数的返回类型定义出来，这样MonadFix的实现能写的简洁一点：

```Haskell
data STMret a = STMret (State# RealWorld) a

liftSTM :: STM a -> State# RealWorld -> STMret a
liftSTM (STM m) = \s -> case m s of (# s', r #) -> STMret s' r

mfix k = STM $ \s ->
	let ans = liftSTM (k r) s
    	STMret _ r = ans
	in case ans of STMret s' x -> (# s', x #)
```

至此，我们便完成了STM的抽象。

### 参考资料

魔力Haskell https://github.com/winterland1989/magic-haskell

Real World Haskell http://book.realworldhaskell.org/

Marlow S. Haskell 2010 language report[J].  http://www.haskell.org/

Erkok L. Value recursion in monadic computations[J]. 2002.

Haskell Wiki Typeclassopedia https://wiki.haskell.org/Typeclassopedia

Haskell Package Documentation:

GHC.Ext http://hackage.haskell.org/package/base-4.11.0.0/docs/GHC-Exts.html

Data.Functor http://hackage.haskell.org/package/base-4.11.0.0/docs/Data-Functor.html

Control.Applicative http://hackage.haskell.org/package/base-4.11.0.0/docs/Control-Applicative.htm

Control.Monad http://hackage.haskell.org/package/base-4.11.0.0/docs/Control-Monad.html

Control.Monad.Fix http://hackage.haskell.org/package/base-4.11.1.0/docs/Control-Monad-Fix.html

Control.Monad.Fail http://hackage.haskell.org/package/base-4.11.1.0/docs/Control-Monad-Fail.html

Control.Monad.STM http://hackage.haskell.org/package/stm-2.4.5.0/docs/Control-Monad-STM.html

Monadlib.Derive https://hackage.haskell.org/package/monadLib-3.7.3/docs/MonadLib-Derive.html
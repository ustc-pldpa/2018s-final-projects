# Haskell中STM的原理

### 1. 概要

这一部分根据GHC的说明材料和Harris大神的工作，说明了Haskell中STM的实现原理。

### 2. 准备工作

#### 2.1. TSO

TSO全名是Thread State Object，包括了一个线程的完整状态，当然也包括其栈。TSO生存在堆上，通过垃圾收集来回收。除此之外，TSO包括几个重要字段：link, global_link, what_next, why_blocked, block_info, bound, cap。很多类型都使用和TSO产生互动：例如，对于一个`MVar#`对象，存在一个队列记录着被其阻塞的TSO们。

#### 2.2. TRec

对于一个事务，GHC将其行动保存在`TRec`的记录中，记录包含过程中读写过变量的集合。当事务结束了本地的活动后，需要对比记录中读过的数据和主内存中数据的一致性，然后获取被写变量的锁。最后改变这些变量并释放锁。`TRec`有五种状态：ACTIVE, CONDEMNED, COMMITED, ABORTED, WAITING。其中第二种状态代表发现不一致的情况，其余的几种顾名思义。

### 3. Retry

先假设没有`orElse`的选择问题，`retry#`的机制很像异常处理。我们已经知道每个变量有一个队列来放置被阻塞的TSO们。当一个事务提交的时候，其相应的写TSO就会被唤醒，那么TSO是什么时候插入队列中的呢？是在此之前的验证阶段中。如果验证成功，那么每个访问过的变量就会插入当前的写TSO。因为队列顺序的原因，排在后面的TSO不会比该次的TSO更早的访问这个数据。如果验证失败了，那么就进入`retry`的环节，产生新的`TRec`并从头开始执行事务。

当然，在唤醒TSO的时候还会检查一次一致性，这时的失败更多是由于其它事务导致的——例如临时获得了锁。这样的话事务将会进入ABORTED状态，并且所有产生的TSO都会被移除

### 4. OrElse

允许`orElse`的`retry#`执行的时候，它会在栈上寻找相应的retry catcher帧或者更外层的atomically帧。前者是由`orElse`来放置的处理机制，而后者就是普通程序和事务的分界线。为了实现这样的机制，GHC采用了嵌套的事务结构，即`TRec`中含有子`TRec`的设计。对于每个`TRec`维护的相关变量集合，在进行验证时，考虑自身和父`TRec`；而在retry的时候，只抛弃自身的`TRec`，以及已经插入的TSO。

不过GHC在嵌套上所做的比上面描述的更好，在于如果两个选择都失败后，某个只对其中一个选项相关的变量产生了更改。那么，所要做的肯定是唤醒这个和更改相关的选项进行尝试。为了达成这样的效果，在抛弃子`TRec`相关变量的同时，需要将其记录在父`TRec`上。

### 5. Beauty

我们从前面可以看出来，用户使用的Haskell函数和底层十分的接近。但同时，我们会发现一些函数的行为并没有使用底层的原生机制，比如Control.Concurrent.STM.TVar中的`TVar`并非产生自`TVar#`，而`check`也是一般的函数而非包装自`check#`。这是为了保证逻辑正确的情况下，使得代码更加简洁。因此笔者认为，除非有特殊的用途，**使用者无需关心底层实现的逻辑**，因为所使用的函数可能仅仅是结果上和底层期待的方式一致，而不应该根据理解的底层行为去期待一些中间结果。编写基本库的大神们使用GHC提供的底层函数构建出了可操作的框架，应用者就请尽情的享受函数式带来的遍历。绝大部分的时候，这些接口是*黑魔法*构成的与否，并不会影响程序的正确性。

### 参考资料

Harris T, Marlow S, Peyton-Jones S, et al. Composable memory transactions[C]//Proceedings of the tenth ACM SIGPLAN symposium on Principles and practice of parallel programming. ACM, 2005: 48-60.

Harris T, Jones S P. Transactional memory with data invariants[C]//First ACM SIGPLAN Workshop on Languages, Compilers, and Hardware Support for Transactional Computing (TRANSACT’06), Ottowa. 2006, 92.

Harris T, Larus J, Rajwar R. Transactional memory[J]. Synthesis Lectures on Computer Architecture, 2010, 5(1): 1-263.

GHC Commentary:

STM https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/STM

HeapObjects https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/HeapObjects


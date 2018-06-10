# 15 - The Abstraction of STM in Haskell

+ 题目：软件事务内存在Haskell语言中的抽象
+ 成员：安鸣霄

### 摘要

事务通常被认为是过程性的，因此使用强函数式语言在描述事务并不简单。即便如此，经过了长时间的研究和社区的积累，Haskell完成了对事务的抽象。本报告即调研了软件事务内存STM在Haskell中的原理、实现以及应用。

本报告分成三个部分：第一部分介绍STM所实现的类型类，从而完成对STM的抽象；第二部分通过实例，在比较中强调使用事务的特点和重要性；第三部分解析STM的实现原理。

### 链接

[第一部分：STM的抽象](abstraction.md)

[第二部分：STM的应用](application.md)

[第三部分：STM的原理](principle.md)
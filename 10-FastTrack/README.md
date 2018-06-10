## Global State, Vector Time and Dynamic Race Detection
陈泳舟 PB15111667

## 摘要

这篇报告调研了分布式系统中一个重要的概念：Vector Time。其中包括Vector Time的产生动机，Vector Time如何准确描述分布式系统的时间，Vector Time做为格的偏序集合的一些性质，以及Vector Time可以解决分布式系统中的哪些问题。之后，我们具体到解决多线程模型数据冲突这个问题上，介绍了使用Vector Time的DJIT+算法，和在它之上进行算法优化的FastTrack算法。并介绍了如何使用RoudRunner来检验Java程序中潜在的数据竞争。
# Atomic Memory Accesses in C/C++11 Semantics and Implementation

题目： 原子内存访问在 C/C++11 中的语义和实现

仓库地址：[github.com/noirgif/2018s-final-projects](https://github.com/noirgif/2018s-final-projects)

摘要： C11 和 C++11 (以下简称C/C++11) 规定了原子操作对内存的访问的一致性机制，从弱到强有： relaxed, acquire/release, sequentially consistent。然而由于不同硬件，更精确地说，计算机体系结构的不同，可能会有不同的内存访问模型，而这导致了 C/C++11 的原子操作在各个平台上的实现也不同，甚至出现问题，本报告围绕 Ori Lahav et al. [Repairing Sequential Consistency in C/C++11](https://plv.mpi-sws.org/scfix/paper.pdf) 提出的问题, 从上而下地解释 C/C++11 到各架构处理器的内存访问机制，说明 C/C++11 在 IBM Power 架构上的实现为何是有缺陷的，以及已经提出的解决办法。
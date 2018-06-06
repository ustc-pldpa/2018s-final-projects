# Cambricon: An Instruction Set Architecture for Neural Networks

## 背景

### 神经网络
神经网络(Neural Networks, NN)是广泛运用于机器学习和模式识别的一系列模型的集合。NN被应用于很大范围的不同应用，如模式识别、web搜索等中，已被例证为目前最为先进的深度学习技术，在某些特别的应用中，甚至达到了人类的识别水准。

传统NN技术运行在通用计算处理器，如CPU和GPGPU(Genral Purpose GPU)上，但由于这些通用处理器的设计要求，它们必须能够灵活地支持不同的工作负载，这样以来，在运行NN程序时，额外的硬件资源还是会被浪费在这种“通用支持”上，导致了通用处理器在运行NN程序时出现了效率较低，能源浪费的问题。

因此，为了解决这种问题，面向神经网络应用的专用硬件加速器被设计出来。然而这种加速设备只针对于小部分有相似计算模式的NN技术做了优化，它们使用了一种较为复杂的指令集，这种ISA直接对应上层NN功能块(如卷积层, pooling层等)甚至一整个神经网络模型，而没有实现底层的计算操作(如点积等)。这种ISA虽然可以很方便、直接地执行那些只使用了它所优化了的NN技术的程序，但却因此缺乏灵活性，难以支持更多可能更加完备有效的NN技术——如果想要在原本的基础上增加对新的NN模型/技术的支持，重新进行设计的工作量会非常大；此外，这种ISA的指令译码器的设计和验证复杂度，及其硬件的面积开销/功耗可能会比较难以接受。

一个这种NN加速器的最新例子就是DaDianNao，它可以有效地支持多层感知器(Multi-Layer Perceptrons, MLPs)，但无法支持波尔茨曼机(Boltzmann Machines, BMs，BM中神经网络节点之间全相联，类似一个无向完全图)。

## 寒武纪ISA简介

寒武纪(Cambricon)是一种领域专用指令集系统(domain-specific Instruction Set Architecture)，用于使用在NN加速器中。Cambricon ISA借鉴了RISC ISA的思想，优点是
1. 可以将复杂的描述高层NN模型和功能块的指令简化为底层的计算操作——如点积——的组合。这样以来，加速器就可以拥有更大范围的应用域，而不是局限于某几种特定模型，因为大部分新的NN技术的功能块，都可以使用这些底层的运算操作指令描述出来。
2. 功能简单、长度较短的指令可以有效地减少指令译码器设计和验证的复杂度，以及硬件开销/能耗。

Cambricon是一种load-store型ISA，指令集中包含了标量、向量、矩阵、逻辑计算，数据迁移和控制指令，是基于对现有NN技术的综合分析设计而成。Cambricon ISA中所有的指令都为64位，有64个32位通用寄存器(General-Purpose Registers, GPRs)，这些通用寄存器主要用于控制和计算地址。为了支持对向量/矩阵数据的运算，综合效率和能耗的考虑，寒武纪体系结构中并没有使用向量寄存器，而是使用了程序员/编译器不可见的片上高速暂存存储器(on-chip scratchpad memory)。片上存储器不需要像寄存器文件一样实现多个端口，而是可以将内存组织为多个独立的banks，根据块号(内存地址的低位)来进行多体交叉编址，在访存向量/矩阵数据时进行并行访问，以提高访存效率[1]。如果采用一般SIMD体系结构的做法，使用向量寄存器来进行向量的存储，整体的计算效率就会受制于寄存器的长度，而片上缓存中的bank宽度很容易就可以做到比寄存器宽度大，因此Cambricon可以支持较大的向量/长度不定的向量。
![Four-way interleaved memory banks using block addressing](https://github.com/wwqqqqq/2018s-final-projects/raw/master/figures/1.png)


实验显示，寒武纪对于大部分NN技术都具有很强的描述能力，相比于通用ISA如x86、MIPS、GPGPU等，具有更高的代码密度。在十种不同的NN benchmark测试中，相比于在此之前的最新NN加速器DaDianNao (只能很好地支持3中NN技术)，基于寒武纪的加速器只产生了可忽略不计的延迟/功率/面积开销。


# 参考文献
[1] Computer Architechture: A Quantitative Approach, Fifth Edition. Hennessy, J.L., Patterson, D.A. 
# Cambricon: An Instruction Set Architecture for Neural Networks

## 设计背景

神经网络(Neural Networks, NN)是广泛运用于机器学习和模式识别的一系列模型的集合。NN被应用于很大范围的不同应用，如模式识别、web搜索等中，已被例证为目前最为先进的深度学习技术，在某些特别的应用中，甚至达到了人类的识别水准。

传统NN技术运行在通用计算处理器，如CPU和GPGPU(Genral Purpose GPU)上，但由于这些通用处理器的设计要求，它们必须能够灵活地支持不同的工作负载，这样以来，在运行NN程序时，额外的硬件资源还是会被浪费在这种“通用支持”上，导致了通用处理器在运行NN程序时出现了效率较低，能源浪费的问题。

因此，为了解决这种问题，面向神经网络应用的专用硬件加速器被设计出来。然而这种加速设备只针对于小部分有相似计算模式的NN技术做了优化，它们使用了一种较为复杂的指令集，这种ISA直接对应上层NN功能块(如卷积层, pooling层等)甚至一整个神经网络模型，而没有实现底层的计算操作(如点积等)。这种ISA虽然可以很方便、直接地执行那些只使用了它所优化了的NN技术的程序，但却因此缺乏灵活性，难以支持更多可能更加完备有效的NN技术——如果想要在原本的基础上增加对新的NN模型/技术的支持，重新进行设计的工作量会非常大；此外，这种ISA的指令译码器的设计和验证复杂度，及其硬件的面积开销/功耗可能会比较难以接受。

一个这种NN加速器的最新例子就是DaDianNao，它可以有效地支持多层感知器(Multi-Layer Perceptrons, MLPs)，但无法支持波尔茨曼机(Boltzmann Machines, BMs，BM中神经网络节点之间全相联，类似一个无向完全图)。

## 寒武纪简介

寒武纪(Cambricon)是一种领域专用指令集系统(domain-specific Instruction Set Architecture)，用于使用在NN加速器中。Cambricon ISA借鉴了RISC ISA的思想，优点是
1. 可以将复杂的描述高层NN模型和功能块的指令简化为底层的计算操作——如点积——的组合。这样以来，加速器就可以拥有更大范围的应用域，而不是局限于某几种特定模型，因为大部分新的NN技术的功能块，都可以使用这些底层的运算操作指令描述出来。
2. 功能简单、长度较短的指令可以有效地减少指令译码器设计和验证的复杂度，以及硬件开销/能耗。

Cambricon是一种load-store型ISA，指令集中包含了标量、向量、矩阵、逻辑计算，数据迁移和控制指令，是基于对现有NN技术的综合分析设计而成。Cambricon ISA中所有的指令都为64位，有64个32位通用寄存器(General-Purpose Registers, GPRs)，这些通用寄存器主要用于控制和计算地址。为了支持对向量/矩阵数据的运算，综合效率和能耗的考虑，寒武纪体系结构中并没有使用向量寄存器，而是使用了程序员/编译器不可见的片上高速暂存存储器(on-chip scratchpad memory)。片上存储器不需要像寄存器文件一样实现多个端口，而是可以将内存组织为多个独立的banks，根据块号(内存地址的低位)来进行多体交叉编址，在访存向量/矩阵数据时进行并行访问，以提高访存效率[1]。如果采用一般SIMD体系结构的做法，使用向量寄存器来进行向量的存储，整体的计算效率就会受制于寄存器的长度，而片上缓存中的bank宽度很容易就可以做到比寄存器宽度大，因此Cambricon可以支持较大的向量/长度不定的向量。
![Four-way interleaved memory banks using block addressing](https://github.com/wwqqqqq/2018s-final-projects/raw/master/figures/1.png)
<center>figure 1. Four-way interleaved memory banks using block addressing</center>




实验显示，寒武纪对于大部分NN技术都具有很强的描述能力，相比于通用ISA如x86、MIPS、GPGPU等，具有更高的代码密度。在十种具有代表性的不同的NN技术(MLP, CNN, RNN, LSTM, Autoencoder, Sparse Autoencoder, BM, RBM, SOM, HNN)的benchmark测试中，Cambricon的代码密度为MIPS的13.38倍，x86的9.86倍，GPGPU的6.41倍。此外，相比于在此之前的最新NN加速器DaDianNao (只能很好地支持3中NN技术)，基于寒武纪的加速器只产生了可忽略不计的延迟/功率/面积开销，分别为4.5%，4.4%和1.6%。(???)

### 设计准则
设计目标是实现一个简洁、灵活、高效，且适用于NN的ISA，通过分析不同NN技术的计算操作和内存访问模式，确定了如下的几个设计准则：
* 数据级并行(Data-Level Parallelism, DLP)
    并行是提高效率的有效方法，计算机中，并行可以分为请求级并行、进程级并行、线程级并行、数据级并行和指令级并行[1]。其中，请求级并行(多个任务可被分配到多个计算机上并行执行)、进程级并行(进程可被调度到不同的处理器上并行执行)、线程级并行(任务被组织成多个共享地址空间的线程)都需要依靠操作系统、编译器或程序员调度，超过了ISA设计的要求，这里主要考虑数据级并行和指令级并行。
    * 数据级并行：指单线程(逻辑上)中并行处理多个数据(SIMD/Vector execution)，每个线程使用一个程序计数器和多个执行部件。
    * 指令级并行：针对单一指令流，多个执行部件并行执行不同的指令

    经过研究，发现大多数NN技术将神经网络上的"神经元"和"突触"数据组织成一个层(layer)，然后以统一/对称的方式进行操作。当执行这些操作时，由向量/矩阵指令启用的数据级并行性可能比传统标量指令集具有更高的并行度和效率，同时具有更高的代码密度。因此，Cambricon的实现中，将注重于数据级并行的实现。
* 定制向量/矩阵计算指令
    虽然目前已经有很多支持大部分科学计算的线性代数运算库(BLAS，LAPACK等)，但对于NN技术，这些现有库中定义的基本操作却不一定是最高效的选择。首先，这些库中涵盖的操作很多，有的操作和计算可能是多余或低效的，更重要的是，这些传统的线性代数库中没有覆盖NN中的许多常见操作，如BLAS库中不支持向量按元素的指数计算，也不支持随机向量的生成(在突触初始化、dropout和受限玻尔兹曼机(RBM)中都有应用)。因此，有必要全面地为现有的NN技术定制一组小而有代表性的向量/矩阵指令集，而不是简单地调用现有的线性代数库。
* 使用片上暂存存储器
    实验发现，NN技术通常需要对向量/矩阵数据进行密集、连续和可变长的访问，因此使用固定宽度、高能耗、价格高的向量寄存器不是最具性价比的选择。Cambricon使用片上暂存器代替了向量寄存器，对每个向量/矩阵数据提供了灵活的可访问宽度。同时，由于神经网络中的突触数据通常很大，且很少被重复使用，所以即使使用向量寄存器，带来的性能增益也不会太多，因此，使用片上暂存器也是考虑数据级并行的高效选择了。

### 寒武纪指令集概览
整体上看，Cambricon包含四种指令：计算指令、逻辑指令、控制指令、数据传送指令。尽管每种指令需要的指令长度不同，但这里采用RISC指令集的思想，同时处于内存对齐和设计难度的考虑，将指令长度固定在64位。下表为Cambricon指令集的指令类型、支持操作数概览：
![An overview to Cambricon instructions](https://github.com/wwqqqqq/2018s-final-projects/raw/master/figures/2.png)
<center>Table 1. An overview to Cambricon instruction.</center>

Cambricon中的控制指令和数据传送指令很大程度上都类似MIPS指令集，但对于NN技术做出了一定的优化。

#### 控制指令
类似于MIPS，Cambricon中有两种控制指令：跳转和条件分支指令，指令格式如图2所示。跳转指令通过一个立即数或通用寄存器来指令地址偏移，使程序跳转到`PC + {offset}`指定的地址位置。条件分支指令除了使用立即数或通用寄存器来指定地址偏移量外，还使用一个通用寄存器来确定是否跳转，如下图中的`Reg0`，指令译码/执行时，通过比较`Reg0`的值和0来判断是否跳转，如果确定跳转(branch taken)，跳转到`PC + {offset}`指定的地址，否则跳转到`PC + 1`。

![Jump instruction and condition branch instruction](https://github.com/wwqqqqq/2018s-final-projects/raw/master/figures/3.png)
<center>Figure 2. <I>top</I>:Jump instruction. <I>bottom</I>: Condtion Branch (CB) instruction.</center>

#### 数据传送指令
Cambricon中的数据传送指令支持不同的数据大小以实现对于向量/矩阵的计算/逻辑指令的灵活支持。这些指令可以load/store可变长的数据块——数据块的大小可以由数据传送指令中的数据宽度操作数指定(如图3中`V_size`部分)。数据传送操作可以发生在主存和片上暂存器之间，或片上暂存器和标量通用寄存器之间。

图3中是VLOAD(Vector LOAD)指令的指令格式，VLOAD指令可以按`V_size`指令的数据块大小从主存到片上暂存器上传送数据，主存中数据的源地址为通用寄存器`Reg2`中所存数据和立即数`Immed`的和。其他数据传送指令，如VSTORE(Vector STORE)、MLOAD(Matrix LOAD)、MSTORE(Matrix STORE)的格式与VLOAD相同。

![Vector Load (VLOAD) instruction](https://github.com/wwqqqqq/2018s-final-projects/raw/master/figures/4.png)
<center>Figure 3. Vector Load (VLOAD) instruction.</center>


# 参考文献
[1] Computer Architechture: A Quantitative Approach, Fifth Edition. Hennessy, J.L., Patterson, D.A. 
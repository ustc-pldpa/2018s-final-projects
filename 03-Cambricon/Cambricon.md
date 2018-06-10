# Cambricon: An Instruction Set Architecture for Neural Networks

[TOC]

## 设计背景

神经网络(Neural Networks, NN)是广泛运用于机器学习和模式识别的一系列模型的集合。NN被应用于很大范围的不同应用，如模式识别、web搜索等中，已被例证为目前最为先进的深度学习技术，在某些特别的应用中，甚至达到了人类的识别水准。

传统NN技术运行在通用计算处理器，如CPU和GPGPU(Genral Purpose GPU)上，但由于这些通用处理器的设计要求，它们必须能够灵活地支持不同的工作负载，这样以来，在运行NN程序时，额外的硬件资源还是会被浪费在这种“通用支持”上，导致了通用处理器在运行NN程序时出现了效率较低，能源浪费的问题。

因此，为了解决这种问题，面向神经网络应用的专用硬件加速器被设计出来。然而这种加速设备只针对于小部分有相似计算模式的NN技术做了优化，它们使用了一种较为复杂的指令集，这种ISA直接对应上层NN功能块(如卷积层, pooling层等)甚至一整个神经网络模型，而没有实现底层的计算操作(如点积等)。这种ISA虽然可以很方便、直接地执行那些只使用了它所优化了的NN技术的程序，但却因此缺乏灵活性，难以支持更多可能更加完备有效的NN技术——如果想要在原本的基础上增加对新的NN模型/技术的支持，重新进行设计的工作量会非常大；此外，这种ISA的指令译码器的设计和验证复杂度，及其硬件的面积开销/功耗可能会比较难以接受。

一个这种NN加速器的最新例子就是DaDianNao，它可以有效地支持多层感知器(Multi-Layer Perceptrons, MLPs)，但无法支持波尔茨曼机(Boltzmann Machines, BMs，BM中神经网络节点之间全相联，类似一个无向完全图)。

## 寒武纪简介

寒武纪(Cambricon)是一种领域专用指令集系统(domain-specific Instruction Set Architecture)，用于使用在NN加速器中。Cambricon ISA借鉴了RISC ISA的思想，优点是
1. 可以将复杂的描述高层NN模型和功能块的指令简化为底层的计算操作——如点积——的组合。这样以来，加速器就可以拥有更大范围的应用域，而不是局限于某几种特定模型，因为大部分新的NN技术的功能块，都可以使用这些底层的运算操作指令描述出来。
2. 功能简单、长度较短的指令可以有效地减少指令译码器设计和验证的复杂度，以及硬件开销/能耗。

Cambricon是一种load-store型ISA，指令集中包含了标量、向量、矩阵、逻辑计算，数据迁移和控制指令，是基于对现有NN技术的综合分析设计而成。Cambricon ISA中所有的指令都为64位，有64个32位通用寄存器(General-Purpose Registers, GPRs)，这些通用寄存器主要用于控制和计算地址。为了支持对向量/矩阵数据的运算，综合效率和能耗的考虑，寒武纪体系结构中并没有使用向量寄存器，而是使用了程序员/编译器可见的片上高速暂存存储器(on-chip scratchpad memory)。片上存储器不需要像寄存器文件一样实现多个端口，而是可以将内存组织为多个独立的banks，根据块号(内存地址的低位)来进行多体交叉编址，在访存向量/矩阵数据时进行并行访问，以提高访存效率[1]。如果采用一般SIMD体系结构的做法，使用向量寄存器来进行向量的存储，整体的计算效率就会受制于寄存器的长度，而片上缓存中的bank宽度很容易就可以做到比寄存器宽度大，因此Cambricon可以支持较大的向量/长度不定的向量。
![Four-way interleaved memory banks using block addressing](https://github.com/wwqqqqq/2018s-final-projects/raw/master/figures/1.png)
<center>figure 1. Four-way interleaved memory banks using block addressing</center>



实验显示，寒武纪对于大部分NN技术都具有很强的描述能力，相比于通用ISA如x86、MIPS、GPGPU等，具有更高的代码密度。在十种具有代表性的不同的NN技术(MLP, CNN, RNN, LSTM, Autoencoder, Sparse Autoencoder, BM, RBM, SOM, HNN)的benchmark测试中，Cambricon的代码密度为MIPS的13.38倍，x86的9.86倍，GPGPU的6.41倍。此外，相比于在此之前的最新NN加速器DaDianNao (只能很好地支持3中NN技术)，基于寒武纪的加速器只产生了可忽略不计的延迟/功率/面积开销，分别为4.5%，4.4%和1.6%。

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

由于使用了片上暂存器而不是向量寄存器，向量/矩阵大小是可变的，但使用时需注意`V_size`不能超出片上暂存器的容量，如果超出，编译器会将较长的向量/矩阵分成若干较短的块，并产生多条指令去处理它们。

向量和矩阵指令的片上暂存器容量在Cambricon中是固定的。在Cambricon中，对于向量指令，存储器容量为64KB，对于矩阵指令，存储器容量为768KB。然后，前面提过，Cambricon通过将内存分成多个体(banks)来提高访存并行度，Cambricon虽然固定了片上暂存器的容量，却没有限制暂存器中的bank数目，为微架构级的实现留下了很多的自由。

#### 计算/逻辑指令

在神经网络系统中，大多数算术运算(如加法、乘法、激活函数等)都可以聚合为矢量运算，根据对目前最为前沿的卷积神经网络GoogLeNet的量化研究，这个比例可以达到99.992%，此外，GoogLeNet中99.791%的向量运算(如点积运算)可以进一步聚合为矩阵运算(如向量-矩阵乘法)。所以，神经网络可以自然地分解为标量、向量和矩阵运算。此外，ISA设计时，必须有效地利用潜在的数据级并行性和数据的局部性。

### 计算/逻辑指令详解

#### 矩阵指令
经过对现有NN技术的全面调研，Cambricon共设计了6条矩阵指令。

下面，用一个很有代表性的神经网络MLP(多层感知器, Multi-Level Perceptrons)为例，详细解释这些矩阵指令是怎么做到对NN的支持的。

多层感知器（MLP）是一类前馈人工神经网络。一个MLP至少由三层节点组成，每一层都根据值已知的输入神经元，来计算输出神经元的值。除输入节点外，每个节点都是使用非线性激活函数的神经元。 MLP利用称为反向传播的监督学习技术进行训练。[2] [3]其多层和非线性激活将MLP与线性感知器区分开来。它可以区分不能线性分离的数据。[4]

MLP由三层或更多层（具有一个或多个隐藏层的输入层和输出层）的非线性激活节点组成。由于MLP是全连接(fully-connected)的，因此一层中的每个节点都以一定权重$ w_{ij} $连接到下一层中的每个节点。图4中即为这样的一个层的前馈运行示意。

![Typical operations in NNs](https://github.com/wwqqqqq/2018s-final-projects/raw/master/figures/5.png)
<center>Figure 4. Typical operations in NNs.</center>

输出神经元$ y_i $ (*i* = 1, 2, 3)可以由以下公式计算:

![](http://latex.codecogs.com/gif.latex?y_i=f(\\sum_{j=1}^3w_{ij}x_j+b_i))

这里的x<sub>j</sub>为第j个输入神经元，w<sub>ij</sub>为第i个输出神经元和第j个输入神经元之间的权重，b<sub>i</sub>为第i给输出神经元对应的偏置(bias)，*f*为激活函数。

输出神经元的计算还可以转化为向量运算：

![](http://latex.codecogs.com/gif.latex?\\vec{y}=\\vec{f}(W\\vec{x}+\\vec{b})\\tag{1})

其中，**y** = (y<sub>1</sub>, y<sub>2</sub>, y<sub>3</sub>)，**x** = (x<sub>1</sub>, x<sub>2</sub>, x<sub>3</sub>)，**b** = (b<sub>1</sub>, b<sub>2</sub>, b<sub>3</sub>)，分别为输出神经元的值的向量，输入神经元向量，和输入神经元对应的bias。W = (w<sub>ij</sub>)为权值矩阵，**f**为每个元素对应的激活函数*f*。

公式(1)中的关键步骤是计算W**x**，在Cambricon中。这一步由`Maxtrix-Mult-Vector`(MMV)指令完成，该指令格式如图5所示。
![Matrix Mult Vector (MMV) instruction](https://github.com/wwqqqqq/2018s-final-projects/raw/master/figures/6.png)
<center>Figure 5. Matrix Mult Vector (MMV) instruction.</center>

`Reg0`存放输出向量的内存基地址(存储在片上暂存器中)`Vout_addr`；`Reg1`为输出向量的大小`Vout_size`；`Reg2`, `Reg3`, `Reg4`分别存有输入矩阵的基地址`Min_addr`，输入向量的基地址`Vin_addr`和输入向量的大小`Vin_size`，`Vin_size`在不同的指令中是可变的。

MMV指令支持任意比例的矩阵-向量乘法，只要所有的输入、输出数据都可以同时被存放在暂存存储器中。

在Cambricon中，使用MMV指令来计算W**x**，而不是将这一操作分解成多个向量点乘，因为后者对于矩阵M的不同行重复地使用了向量**x**，带了了很多额外的问题，如同步、对同一地址的并发读写请求等，降低了操作的效率。

然而，MMV指令不提供对于反向训练(backforward training)过程的有效支持。反向传播(Back Propagation, BP)是一种用于人工神经网络的方法，它的关键步骤即为计算梯度向量，该向量又被用来计算神经网络中的权重，即使用梯度下降算法，用来通过计算损失函数的梯度来调整神经元的权重。[5]

梯度向量可以被表示为一个向量乘以一个矩阵。如果要使用MMV指令来完成这种计算，还需要一条额外的矩阵转置指令来实现，而矩阵转置需要的数据在内存中移动的开销较大，为此，Cambricon中还实现了一条`Vector-Mult-Matrix`(VMM)指令，VMM指令即可直接用于反向传播算法的backforward训练过程。VMM指令的格式和MMV相同，操作码不同。

此外，在训练神经网络时，权重矩阵W通常需要用公式![](http://latex.codecogs.com/gif.latex?W=W+\\eta\\,\\Delta\\,W)来进行更新，其中![](http://latex.codecogs.com/gif.latex?\\eta)为学习速度(learning rate)，![](http://latex.codecogs.com/gif.latex?\\Delta\\,W)为两个向量的外积。为了高效实现权值的更新，Cambricon中提供了`Outer-Product`(OP)指令、`Matrix-Mult-Scalar`(MMS)指令、`Matrix-Add-Matrix`(MAM)指令来进行协同操作。

此外，Cambricon中还有一个`Matrix-Subtract-Matrix`(MSM)指令来更好地支持受限波尔茨曼机(Restricted Boltzmann Machine, RBM)中的权重更新。

故Cambricon中的6个矩阵计算指令有：MMV, VMM, OP, MMS, MAM, MSM.

#### 向量指令
仍然以公式(1)为例，可以看出上一节中的矩阵指令不足以定义该公式中的所有计算。例如，W**b**的结果和**b**都为向量，同时对W**x**+**b**的结果也需要做一个逐向量的映射**f**。对于向量加法W**x**+**b**，Cambricon中有直接支持的指令`Vector-Add-Vector`(VAV)，但对于逐向量的激活函数(element-wise activation)，还是需要多条指令才能完成。这里使用一个非常常用的激活函数sigmoid函数为例来加以说明。

Sigmoid函数由以下公式定义：

![](http://latex.codecogs.com/gif.latex?S(x)=\\frac{1}{1+e^{-x}}=\\frac{e^x}{e^x+1})

对输入向量**a**执行sigmoid激活函数可以分解为3个连续步骤，这三个步骤分别由3条指令支持：
| 步骤 | Cambricon中对应的指令 |
|----- | ------ |
|对于**a**中的每个元素，计算![](http://latex.codecogs.com/gif.latex?e^{a_i})，i = 1, ..., n | `Vector-Exponential` (VEXP) |
| 将向量![](http://latex.codecogs.com/gif.latex?(e^{a_i},...,e^{a_n}))中的每个元素加1 | `Vector-Add-Scalar`(VAS) |
| 对于每个i, i = 1, ..., n，计算![](http://latex.codecogs.com/gif.latex?\\frac{e^{a_i}}{e^{a_i}+1})的值 | `Vector-Div-Vector`(VDV) |

不过尽管非常常用，sigmoid函数并不是现有NN技术使用的唯一的激发函数，为了支持多种不同的激发函数，Cambricon还提供了一系列的向量算术指令，如`Vector-Mult-Vector`(VMV), `Vector-Sub-Vector`(VSV), `Vector-Logarithm`(VLOG)。

在计算对数函数、三角函数、反三角函数等超越函数时，可以使用CORDIC算法(Coordinate Rotation Digital Computer, or Volder's algorithm)，CORDIC算法通常在没有硬件乘法器可用时使用，因为它所需要的唯一操作是加法、减法、位移和查表操作[6][7][8][9]。所以，在设计硬件加速器时，使用CORDIC算法，即可使与不同超越函数相关的指令，得以有效地重复使用相同的功能块(包括加减法、移位、查表操作等)。

此外，随机生成向量也是在很多NN技术(如dropout、random sampling)应用的重要操作，但在很多科学计算定义的传统线性代数库(如BLAS)中，都忽视了这个操作。Cambricon提供了一个指令`Random-Vector`(RV)来生成一个随机向量，向量元素的值的生成符合在区间[0, 1]上的均匀分布。有了可以实现均匀分布随机的向量，使用Ziggurat算法，结合其他的向量算术指令和向量比较指令，可以在此基础上实现其他的分布，如高斯分布等。[10]

#### 逻辑指令

很多最先进的NN技术都使用了一些结合了比较等逻辑操作的技术，如max-pooling操作(见图6.a)，它在一个pooling窗口中取其中具有最大输出的神经元，并且在不同的输入特征映射中，对所有对应的pooling窗口重复这一操作，见图6.b。

![Max-pooling operation](https://github.com/wwqqqqq/2018s-final-projects/raw/master/figures/7.png)
<center>Figure 6. Max-pooling operation.</center>

Cambricon中使用`Vector-Greater-Than-Merge`(VGTM)指令来帮助实现max-pooling操作。VGTM指令通过比较输入向量`Vin0`和`Vin1`中的对应元素，来指定输出向量(`V_out`)中的所有元素：
```C
Vout[i] = (Vin0[i] > Vin1[i])? Vin0[i] : Vin1[i];
```
![Vector Greater Than Merge (VGTM) instruction](https://github.com/wwqqqqq/2018s-final-projects/raw/master/figures/9.png)
<center>Figure 7. Vector Greater Than Merge (VGTM) instruction.</center>

下面即为pooling操作的Cambricon汇编实现：
![Pooling code](https://github.com/wwqqqqq/2018s-final-projects/raw/master/figures/8.png)

除了向量计算指令外，Cambricon还提供了一些列向量比较指令(`Vector-Greater-than`(VGT), `Vector-Equal`(VE), `Vector AND/OR/NOT`(VAND/VOR/VNOT))，标量比较指令，及标量逻辑指令来计算分支条件。




## 参考文献
[1] Hennessy, J.L., Patterson, D.A. Computer Architechture: A Quantitative Approach, Fifth Edition. 

[2] Rosenblatt, Frank. x. Principles of Neurodynamics: Perceptrons and the Theory of Brain Mechanisms. Spartan Books, Washington DC, 1961

[3] Rumelhart, David E., Geoffrey E. Hinton, and R. J. Williams. "Learning Internal Representations by Error Propagation". David E. Rumelhart, James L. McClelland, and the PDP research group. (editors), Parallel distributed processing: Explorations in the microstructure of cognition, Volume 1: Foundation. MIT Press, 1986.

[4] Cybenko, G. 1989. Approximation by superpositions of a sigmoidal function Mathematics of Control, Signals, and Systems, 2(4), 303–314.

[5] Deep Learning; Ian Goodfellow, Yoshua Bengio, Aaaron Courville; MIT Press; 2016, p 196.

[6] V. Kantabutra. On hardware for computing exponential and trigonometric functions. Computers, IEEE Transactions on, 1996.

[7] Volder, Jack E. (1959-03-03). "The CORDIC Computing Technique". Proceedings of the Western Joint Computer Conference (WJCC) (presentation). San Francisco, California, USA: National Joint Computer Committee (NJCC): 257–261. Retrieved 2016-01-02.

[8] Volder, Jack E. (1959-05-25). "The CORDIC Trigonometric Computing Technique". IRE Transactions on Electronic Computers. The Institute of Radio Engineers, Inc. (IRE) (published September 1959). 8 (3): 330–334 (reprint: 226–230). EC-8(3):330–334. Retrieved 2016-01-01.

[9] Swartzlander, Jr., Earl E. (1990). Computer Arithmetic. 1 (2 ed.). Los Alamitos: IEEE Computer Society Press. ISBN 9780818689314. 0818689315. Retrieved 2016-01-02.

[10] G Marsaglia and W W. Tsang. The ziggurat method for generating
random variables. Journal of statistical software, 2000.
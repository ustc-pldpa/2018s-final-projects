# DataCollider: Effective Data-Race Detection for the Kernel

## Team member

刘伟森 PB15111595  
王泽凡 PB15111593  

## GitHub Address

https://github.com/LWSen/2018s-final-projects/tree/master/13-data-collider

## Abstract

DataCollider是一个动态检测内核模块中数据竞争的轻量级工具。它能够绕开传统的程序分析方法,利用现有硬件结构中的调试寄存器对复杂的内核代码进行有效的数据竞争检测,并通过采样少量的访存操作来降低系统的运行时开销。  

## Issues and Ideas

* 如果存在数据竞争，那就制造机会让它发生
* 使用代码断点和数据断点捕获数据竞争现场
* 随机采样减少数据竞争检测带来的额外开销

## Reference

[OSDI 2010][DataCollider](https://www.usenix.org/conference/osdi10/effective-data-race-detection-kernel)


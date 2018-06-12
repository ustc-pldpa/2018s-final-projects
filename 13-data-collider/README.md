# DataCollider: Effective Data-Race Detection for the Kernel

## Team member

刘伟森 PB15111595
王泽凡 PB15111593

## GitHub Address

https://github.com/LWSen/2018s-final-projects

## Abstract

DataCollider是一个动态检测内核模块中数据竞争的轻量级工具。它能够绕开传统的程序分析方法,利用现有硬件结构中的调试寄存器对复杂的内核代码进行有效的数据竞争检测,并通过采样少量的访存操作来降低系统的运行时开销。  

## Issues and Ideas

DataCollider的主要思想是如果存在数据竞争，那制造机会就让它发生。  


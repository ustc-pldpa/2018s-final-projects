# 03-Cambricon

## Team Member
| Name | Sno | Github |
|------|-----|--------|
| 韦清 | PB15000027 | [wwqqqqq](https://github.com/wwqqqqq) |

## Introduction
Cambricon is a novel domain-specific Instruction Set Architecture for Neural Network accelerators. It is a load-store architecture that integrates scalar, vector, matrix, logical, data transfer, and control instructoins, based on a comprehensive analysis of existing NN techniques.

## Content of this project
- [Cambricon.md](https://github.com/wwqqqqq/2018s-final-projects/blob/master/03-Cambricon/Cambricon.md)
    Introduction of Cambricon ISA according to the paper: [ISCA 2016] [Cambricon: An Instruction Set Architecture for Neural Networks](https://ieeexplore.ieee.org/document/7551409/?reload=true)    
    This report is consist of 4 parts:
    * Background
    * Overview of Cambricon
    * Instruction Set
        * Control Instructions
        * Data Transfer Instructions
        * Matrix Instructions
        * Vector Instructions
        * Logical Instructions
        * Scalar Instructions
    * Prototype Accelerator

- [example_code.asm](https://github.com/wwqqqqq/2018s-final-projects/blob/master/03-Cambricon/example_code.asm)
    Cambricon program fragments of MLP, pooling and BM.

## Reference
[ISCA 2016] [Cambricon: An Instruction Set Architecture for Neural Networks](https://ieeexplore.ieee.org/document/7551409/?reload=true)  
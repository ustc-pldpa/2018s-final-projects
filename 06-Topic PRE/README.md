# 11-PRE

## Team Members

| name   | SNo        | Github                                          |
| ------ | ---------- | ----------------------------------------------- |
| 宋小牛 | PB15000301 | [Jeffery-Song](https://github.com/Jeffery-Song) |

## Introduction

Partial Redundancy Elimination is an important component in modern compilers. It's a powerful and indispensable optimization. There are computationally and life-time optimal strategy for "Safe PRE" and "Speculative PRE", based on bit-vector or SSA.

In this project, I did some research on optimal PRE algorithms in various forms: *safe* or *speculative*, *bit-vector* or *SSA*.

## Content

* []() is the slides 

## Reference

PRE

* Morel, Etienne, and Claude Renvoise. Global optimization by suppression of partial redundancies. Communications of the ACM 22.2 (1979): 96-103. 

LCM

* J. Knoop, O. Rüthing, and B. Steffen. Lazy code motion. In *Proceedings of the ACM SIGPLAN’92 Conference on Programming Language Design and Implementation*, pages 224–234, 1992
* J. Knoop, O. Rüthing, and B. Steffen. Optimal code motion: theory and practice. *ACM Trans. Program. Lang. Syst.*, 16(4):1117–1155, 1994.

Safe PRE

* K. Kennedy. Safety of code motion. *International Journal of Computer Mathematics*, 3(2 and 3):117–130, 1972.

MC-PRE

* Q. Cai and J. Xue. Optimal and efficient speculation-based partial redundancy elimination. In *Proceedings of the 2th annual IEEE/ACM international symposium on Code generation and optimization*, pages 91–102, 2003.

SSAPRE

* R. Kennedy, S. Chan, S. Liu, R. Lo, P. Tu, and F. Chow. Partial redundancy elimination in SSA form. *ACM Trans. Program. Lang. Syst.*, 21(3):627–676, 1999.
* F. Chow, S. Chan, R. Kennedy, S. Liu, R. Lo, and P. Tu. A new algorithm for partial redundancy elimination based on SSA form. In *Proceedings of the ACM SIGPLAN ’97 Conference on Programming Language Design and Implementation*, pages 273–286, 1997.

SSA

* R. Cytron, J. Ferrante, B. K. Rosen, M. N. Wegman, and F. K. Zadeck. Efficiently computing static single assignment form and the control dependence graph. *ACM Trans. Program. Lang. Syst.*, 13(4):451–490, 1991. ISSN 0164-0925

MC-SSAPRE

* Hucheng Zhou, Wenguang Chen, Fred C. Chow. An SSA-based algorithm for optimal speculative code motion under an execution profile. *PLDI* 2011 
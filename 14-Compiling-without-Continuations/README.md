Project Name: Compiling without Continuations
=
Team Member
-
name | SNo
---|---
梁聪 | PB15000303  

Introduction
-
编译器通常会将高级语言转化为一些特定的中间语言从而在中间语言上对程序进行优化，传统的中间语言有**CPS**形式和**A-Normal Form**形式等。  
对于一类特定的控制流结构：  
```
let f = rhs in  
  case p of A → f 1  
            B → f 2
```
传统的转化方法是将 **f** 视为一个独立的*function*或者一个*continuation*，这样会引入不必要的内存开销或者使优化变得困难。通过在中间语言中专门定义**Join Point**来代表 **f** 这样的控制流汇合点代码，不会引入大量冗余代码或者额外内存开销，同时使**Join Point**处的语句结构保持简洁利于优化。  

Issues and Ideas
-
对于类似
> if ( if e1 then e2 else e3) then e4 else e5  

的结构引入**join points**来表示**e4**和**e5**，从而进行代码优化

Report
-
[pdf](https://github.com/lc150303/2018s-final-projects/blob/master/14-Compiling-without-Continuations/report.pdf)  
[ppt](https://github.com/lc150303/2018s-final-projects/blob/master/14-Compiling-without-Continuations/report.pptx)

Reference
-
[Compiling without continuations](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/11/join-points-pldi17.pdf)  
[The essence of compiling with continuations](http://www.cse.csusb.edu/egomez/cs201/pldi-fsdf.pdf)  
[Stream Fusion:From Lists to Streams to Nothing at all](https://github.com/lc150303/fopl-lc-final-project/blob/master/Stream_Fusion_From_Lists_to_Streams_to_Nothing_at_all.pdf)

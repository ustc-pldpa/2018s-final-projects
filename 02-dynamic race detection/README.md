# Project Name: Dynamic Race Detection for C++11

## Team Member

[李嘉豪](https://github.com/gloit042/2018s-final-projects) PB15111671

## Abstract

The intricate rules for memory ordering and synchronisation associated
with the C/C++11 memory model mean that data races
can be difficult to eliminate from concurrent programs. Dynamic
data race analysis can pinpoint races in large and complex applications,
but the state-of-the-art ThreadSanitizer (tsan) tool for
C/C++ considers only sequentially consistent program executions,
and does not correctly model synchronisation between C/C++11
atomic operations. The authors present a scalable dynamic data race analysis
for C/C++11 that correctly captures C/C++11 synchronisation,
and uses instrumentation to support exploration of a class of non sequentially
consistent executions. The authors concisely define the memory
model fragment captured by their instrumentation via a restricted axiomatic
semantics, and show that the axiomatic semantics permits
exactly those executions explored by their instrumentation. 

## issues and ideas

Their work can be extended to detect race in GPU based computation model

## Reference

1. [Dynamic Race Detection for C++11](http://www.doc.ic.ac.uk/~afd/homepages/papers/pdfs/2017/POPL.pdf)
2. [C++ Memory Model](https://en.cppreference.com/w/cpp/language/memory_model)

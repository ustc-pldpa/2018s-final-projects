# Refinement.js 


Yet another contract library for JavaScript which benefits from static analyzers such as TAJS.


## Team

Jinwei Long [@NiceKingWei](https://github.com/NiceKingWei)

## Abstract

Refinement.js is a Javascript code generator which compiles verification conditions (such as `requires`, `ensures` and `assert`) to es5 code. The target code can be checked by static anaylzers such as TAJS to give useful clues to potential bugs. Dynamic checking of verification conditions is supported as well.

## Motivation

JavaScript is an error-prone scripting language with which so many platform APIs integreted in. It is widely used in web and programmers often shoot themselves in the foot. Some static analyzer using the method of abstract intepretation has been made to find potential bugs. For example, TAJS, which means *Type Analyzer for JavaScript*, is such a tool that can point out some common and trivial bugs such as calling an invalid function, reading a invalid property, etc. But some non-trivial contracts do exist in the programming, especially in platform APIs, which usual analyzers won't process. For instance, you can't use a file if it is closed, but analyzer won't treat it as bug because it knows nothing about this restriction. That's why I introduce `refinement.js` to encode nontrivial specifications into JavaScript code that normal analyzers can check.

## Files

|file|description|
|------|---------|
|refinement.js/ | the source code of refinement.js|
|presentation.pdf | presentation|
|report.pdf Report.md | report of the project(not finished yet)|
|Readme.md | description of the project|
|Future.md | ideas of the future work|

## Progress

A prototype has been made, you can try it now!


## Issues and Ideas

* No stack trace can be provided when assertion violation occured.
* The project is highly related with backend analyzer. It's not so easy to support another analyzer.
* Some information provided by analyzer are not accurate because of the change of the original code.
* Built-in functions supported by TAJS are not well-designed, I will concentrate on this problem in the future. Maybe a deterministic abstract interpreter will be introduced.

## Reference

1. Zhang, Zhen. "xWIDL: modular and deep JavaScript API misuses checking based on extended WebIDL." Companion Proceedings of the 2016 ACM SIGPLAN International Conference on Systems, Programming, Languages and Applications: Software for Humanity. ACM, 2016.
1. Jensen, Simon Holm, Anders Møller, and Peter Thiemann. "Type analysis for JavaScript." International Static Analysis Symposium. Springer, Berlin, Heidelberg, 2009.
1. Kashyap, Vineeth, et al. "Type refinement for static analysis of JavaScript." ACM SIGPLAN Notices. Vol. 49. No. 2. ACM, 2013.
1. Jensen, Simon Holm, Anders Møller, and Peter Thiemann. "Interprocedural analysis with lazy propagation." International Static Analysis Symposium. Springer, Berlin, Heidelberg, 2010.
1. Andreasen, Esben, et al. "Improving Tools for JavaScript Programmers." Proc. of International Workshop on Scripts to Programs. Beijing, China:[sn]. 2012.
2. Laviron, Vincent, and Francesco Logozzo. "Refining abstract interpretation-based static analyses with hints." Asian Symposium on Programming Languages and Systems. Springer, Berlin, Heidelberg, 2009.
1. Lehmann, Nico, and Éric Tanter. "Gradual refinement types." ACM SIGPLAN Notices 52.1 (2017): 775-788.
1. Fähndrich, Manuel, and Francesco Logozzo. "Static contract checking with abstract interpretation." International Conference on Formal Verification of Object-Oriented Software. Springer, Berlin, Heidelberg, 2010.
1. Ferrara, Pietro, Francesco Logozzo, and Manuel Fähndrich. "Safer unsafe code for. net." ACM Sigplan Notices 43.10 (2008): 329-346.
1. Cousot, Patrick M., et al. "An abstract interpretation framework for refactoring with application to extract methods with contracts." ACM SIGPLAN Notices. Vol. 47. No. 10. ACM, 2012.
1. Logozzo, Francesco. "Technology for inferring contracts from code." ACM SIGAda Ada Letters. Vol. 33. No. 3. ACM, 2013.
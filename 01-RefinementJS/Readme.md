# Refinement.js 


Yet another contract library for JavaScript which benefits from static analyzers such as TAJS.


## Team

Jinwei Long [@NiceKingWei](https://github.com/NiceKingWei)

## Abstract

Refinement.js is a Javascript code generator which compiles verification conditions (such as `requires`, `ensures` and `assert`) to es5 code. The target code can be checked by static anaylzers such as TAJS to give useful clues to potential bugs. Dynamic checking of verification conditions is supported as well.

## Files

|file|description|
|------|---------|
|refinement.js/ | the source code of refinement.js|
|presentation.pdf | presentation|
|report.pdf Report.md | report of the project(not finished yet)|
|Readme.md | description of the project|

## Progress

A prototype has been made, you can try it now!


## Issues and Ideas

* No stack trace can be provided when assertion violation occured.
* The project is highly related with backend analyzer. It's not so easy to support another analyzer.
* Some information provided by analyzer are not accurate because of the change of the original code.
* Built-in functions supported by TAJS are not well-designed, I will concentrate on this problem in the future. Maybe a deterministic abstract interpreter will be introduced.
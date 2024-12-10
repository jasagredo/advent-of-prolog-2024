# [Advent of Code 2024](https://adventofcode.com/2024) in [Scryer prolog](https://www.scryer.pl/)

Store the input in a file `NUM.txt` (`NUM` padded to two digits with zeroes), then run:
```
> scryer-prolog NUM.pl
Task 1: <SOLUTION1>
Task 2: <SOLUTION2>
% CPU time: <TIME>, <INF> inferences
```

I use this `~/.scryerrc` which problably is overloaded but it does the job:
```
:- use_module(library(arithmetic)).
:- use_module(library(assoc)).
:- use_module(library(between)).
:- use_module(library(charsio)).
:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(debug)).
:- use_module(library(dif)).
:- use_module(library(format)).
:- use_module(library(lambda)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(pio)).
:- use_module(library(reif)).
:- use_module(library(time)).
```

:- use_module(dcgs/dcgs_utils).

mul(XY) --> "mul(", number(X), ",", number(Y), ")", { XY #= X * Y }.

input([X|Xs]) --> ..., mul(X), input(Xs).
input([])     --> ..., call(eos).

part1(F, Sol) :-
    phrase_from_file(input(Xs), F),
    sum_list(Xs, Sol).

input2(_, [])      --> call(eos).
input2(_, Xs)      --> "don't()", input2(dont, Xs).
input2(do, [X|Xs]) --> mul(X),    input2(do, Xs).
input2(_, Xs)      --> "do()",    input2(do, Xs).
input2(Do, Xs)     --> [_],       input2(Do, Xs).

part2(F, Sol) :-
    phrase_from_file(input2(do, Xs), F),
    sum_list(Xs, Sol).

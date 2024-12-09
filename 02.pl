:- use_module(library(dcgs)).
:- use_module(dcgs/dcgs_utils).
:- use_module(fp).

real("02.txt").
sample("02.sample").

report([X]) --> number(X), eol.
report([X|Xs]) --> number(X), " ", report(Xs).

input([]) --> call(eos).
input([X|Xs]) --> report(X), input(Xs).

safe(_, [_]).
safe(G_2, [X,Y|Xs]) :-
    abs(X - Y) #> 0,
    abs(X - Y) #< 4,
    call(G_2, X, Y),
    safe(G_2, [Y|Xs]).

safe(Xs) :- safe((#>), Xs); safe((#<), Xs).

part1(Mode, Sol) :-
    call(Mode, F),
    phrase_from_file(input(Xs), F),
    filter(safe, Xs, Ys),
    length(Ys, Sol).

safe_dampener(Xs) :-
    safe(Xs); select(_, Xs, Xss), safe(Xss).

part2(Mode, Sol) :-
    call(Mode, F),
    phrase_from_file(input(Xs), F),
    filter(safe_dampener, Xs, Zs),
    length(Zs, Sol).

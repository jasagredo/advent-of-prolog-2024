:- use_module(dcgs/dcgs_utils).
:- use_module(dcgs/dcgs_debug).
:- use_module(sort).
:- use_module(library(iso_ext)).

:- dynamic(ordered/2).

% parse

order, "\n" --> "\n".
order --> number(X), "|", number(Y), "\n", { assertz(ordered(X, Y)) }, order.

manual([X]) --> number(X), "\n".
manual([X|Xs]) --> number(X), ",", manual(Xs).

manuals([]) --> call(eos).
manuals([X|Xs]) --> manual(X), manuals(Xs).

input(X) --> order, "\n", manuals(X).

% logic

sublist([], []).
sublist([_|T0], T1) :-
    sublist(T0, T1).
sublist([H|T0], [H|T1]) :-
    sublist(T0, T1).

is_sorted(L) :-
    forall(sublist(L, [X,Y]), \+ ordered(Y, X)).

middle(L, Mid) :-
    length(L1, X),
    length(L2, X),
    append([L1, [Mid], L2], L).

% part 1

if_sorted(L, Acc0, Acc) :-
    is_sorted(L), middle(L,Mid), Acc #= Acc0 + Mid;
    Acc #= Acc0.

part1(F, Sol) :-
    phrase_from_file(input(X), F),
    foldl(if_sorted, X, 0, Sol).

% part 2

sort_then(L, Acc0, Acc) :-
    \+ is_sorted(L),
    mergesort_by(order, L, L1),
    middle(L1, Mid),
    Acc #= Acc0 + Mid;
    Acc #= Acc0.

part2(F, Sol) :-
    phrase_from_file(input(X), F),
    foldl(sort_then, X, 0, Sol).

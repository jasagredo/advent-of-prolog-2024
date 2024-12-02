:- module(sort, [mergesort/2]).

:- use_module(library(lists)).
:- use_module(library(clpz)).

% from https://www.metalevel.at/misc/sorting.pl

mergesort(Ls0, Ls) :-
        length(Ls0, L),
        zcompare(C, L, 1),
        halving(C, L, Ls0, Ls).

halving(<, _, Ls, Ls).
halving(=, _, Ls, Ls).
halving(>, L, Ls0, Ls) :-
        Half #= L // 2,
        length(Lefts0, Half),
        append(Lefts0, Rights0, Ls0),
        mergesort(Lefts0, Lefts),
        mergesort(Rights0, Rights),
        merge(Lefts, Rights, Ls).

merge([], Ys, Ys) :- !.
merge(Xs, [], Xs) :- !.
merge([X|Xs], [Y|Ys], Ms) :-
        (   X @< Y ->
            Ms = [X|Rs],
            merge(Xs, [Y|Ys], Rs)
        ;   Ms = [Y|Rs],
            merge([X|Xs], Ys, Rs)
        ).

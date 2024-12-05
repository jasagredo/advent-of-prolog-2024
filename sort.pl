:- module(sort, [mergesort/2, mergesort_by/3]).

:- use_module(library(lists)).
:- use_module(library(clpz)).

% from https://www.metalevel.at/misc/sorting.pl

:- meta_predicate mergesort_by(2, ?, ?).

mergesort_by(G_2, Ls0, Ls) :-
        length(Ls0, L),
        zcompare(C, L, 1),
        halving(G_2, C, L, Ls0, Ls).

halving(_, <, _, Ls, Ls).
halving(_, =, _, Ls, Ls).
halving(G_2, >, L, Ls0, Ls) :-
        Half #= L // 2,
        length(Lefts0, Half),
        append(Lefts0, Rights0, Ls0),
        mergesort_by(G_2, Lefts0, Lefts),
        mergesort_by(G_2, Rights0, Rights),
        merge(G_2, Lefts, Rights, Ls).

merge(_, [], Ys, Ys) :- !.
merge(_, Xs, [], Xs) :- !.
merge(G_2, [X|Xs], [Y|Ys], Ms) :-
        (   call(G_2, X, Y) ->
            Ms = [X|Rs],
            merge(G_2, Xs, [Y|Ys], Rs)
        ;   Ms = [Y|Rs],
            merge(G_2, [X|Xs], Ys, Rs)
        ).

mergesort(Ls0, Ls) :- mergesort_by((@<), Ls0, Ls).

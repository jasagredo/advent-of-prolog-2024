:- module(fp, [filter/3, flip/3, zipWith/4, take1/2, sublist/2]).

:- meta_predicate filter(1, ?, ?).
:- meta_predicate flip(2, ?, ?).
:- meta_predicate zipWith(3, ?, ?, ?).

flip(G_2, Y, Z) :-
    call(G_2, Z, Y).

zipWith(_, [], [], []).
zipWith(G_3, [X|Xs], [Y|Ys], [Z|Zs]) :-
    zipWith(G_3, Xs, Ys, Zs),
    call(G_3, X, Y, Z).

sublist([], []).
sublist([_|T0], T1) :-
    sublist(T0, T1).
sublist([H|T0], [H|T1]) :-
    sublist(T0, T1).

take1([X|_], [X]).
take1([], []).

filter(_, [], []).
filter(G_1, [X|Xs], ZZs) :-
    filter(G_1, Xs, Zs),
    ( call(G_1, X)
    -> ZZs = [X|Zs]
    ; ZZs = Zs
    ).

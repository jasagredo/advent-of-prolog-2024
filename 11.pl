:- use_module(library(charsio)).
:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(pio)).
:- use_module(library(debug)).
:- use_module(library(tabling)).

% From The Power of Prolog, section Memoization
:- dynamic(memo_/1).

memo(Goal) :-
        (   memo_(Goal)
        ->  true
        ;   once(Goal),
            assertz(memo_(Goal))
        ).

%% Parses a number
number(X) --> number([], X).
number(X, Z) --> [C], { char_type(C, numeric) }, number([C|X], Z).
number(X, Z) --> { length(X, L), L #> 0, reverse(X, X1), number_chars(Z, X1) }.

input([]) --> "\n".
input([X|Xs]) -->  " ", number(X), input(Xs).
input([X|Xs]) -->  number(X), input(Xs).

blink_memo(0, _, 1).
blink_memo(N, 0, Xs) :-
    N #> 0,
    N1 #= N - 1,
    memo(blink_memo(N1, 1, Xs)).
blink_memo(N, X, Xs) :-
    N #> 0,
    X #> 0,
    number_chars(X, Xc),
    length(Xc, L),
    0 #= L mod 2,
    LL #= L // 2,
    length(X1, LL),
    append(X1, X2, Xc),
    N1 #= N - 1,
    number_chars(XX, X1),
    memo(blink_memo(N1, XX, A)),
    number_chars(YY, X2),
    memo(blink_memo(N1, YY, B)),
    Xs #= A + B.
blink_memo(N, X, Xs) :-
    N #> 0,
    X #> 0,
    number_chars(X, Xc),
    length(Xc, L),
    1 #= L mod 2,
    * portray_clause(case_c(N, X)),
    N1 #= N - 1,
    Y #= X * 2024,
    memo(blink_memo(N1, Y, Xs)).

solve_memo(N, F, Solution) :-
    phrase_from_file(input(Xs), F),
    maplist(blink_memo(N), Xs, Scores),
    sum_list(Scores, Solution).

% This tabled version _should_ work but there is a Rust panic happening.

:- table blink_tabled/3.

blink_tabled(0, _, 1) :- !.
blink_tabled(N, 0, Xs) :-
    N #> 0,
    !,
    N1 #= N - 1,
    blink_tabled(N1, 1, Xs).
blink_tabled(N, X, Xs) :-
    N #> 0,
    X #> 0,
    number_chars(X, Xc),
    length(Xc, L),
    (0 #= L mod 2),
    !,
    LL #= L // 2,
    length(X1, LL),
    append(X1, X2, Xc),
    N1 #= N - 1,
    number_chars(XX, X1),
    number_chars(YY, X2),
    blink_tabled(N1, XX, A),
    blink_tabled(N1, YY, B),
    Xs #= A + B.
blink_tabled(N, X, Xs) :-
    N #> 0,
    X #> 0,
    number_chars(X, Xc),
    length(Xc, L),
    1 #= L mod 2,
    N1 #= N - 1,
    Y #= X * 2024,
    blink_tabled(N1, Y, Xs).

solve_tabled(N, F, Solution) :-
    phrase_from_file(input(Xs), F),
    maplist(blink_tabled(N), Xs, Scores),
    sum_list(Scores, Solution).

run :-
    time(
        (
            solve_memo(25, "11.txt", X),
            format("Task 1: ~w~n", [X]),
            solve_memo(75, "11.txt", Y),
            format("Task 1: ~w~n", [Y])
        )
    ),
    halt.

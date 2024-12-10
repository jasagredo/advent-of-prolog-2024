:- use_module(dcgs/dcgs_utils).
:- use_module(fp).

/*
Part 1:
  Filter the list by the sequence being safe and count the remaining elements.

Part 2:
  Filter the list by the sequence being safe or the sequence with one element
  less being safe, then count the remaining elements.
*/

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

part1(Xs, Sol) :-
    filter(safe, Xs, Ys),
    length(Ys, Sol).

safe_dampener(Xs) :-
    safe(Xs); select(_, Xs, Xss), safe(Xss).

part2(Xs, Sol) :-
    filter(safe_dampener, Xs, Zs),
    length(Zs, Sol).

run :-
    time(
        (
            phrase_from_file(input(Xs), "02.txt"),
            part1(Xs, X),
            format("Task 1: ~w~n", [X]),
            part2(Xs, Y),
            format("Task 2: ~w~n", [Y])
        )
    ),
    halt.

:- initialization(run).

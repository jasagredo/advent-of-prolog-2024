:- use_module(library(dcgs)).
:- use_module(library(assoc)).
:- use_module(sort).
:- use_module(dcgs/dcgs_utils).
:- use_module(fp).

input_format([], []) --> call(eos).
input_format([X|Xs], [Y|Ys]) -->
    number(X),
    "   ",
    number(Y),
    eol,
    input_format(Xs, Ys).

distance(X, Y, Z) :-
    Z #= abs(X - Y).

part1(F, Solution):-
    phrase_from_file(input_format(Xs, Ys), F),
    mergesort(Xs, Xss),
    mergesort(Ys, Yss),
    zipWith(distance, Xss, Yss, Zss),
    sum_list(Zss, Solution).

input_format2(Xs, Ys) -->
    call(eos),
    { empty_assoc(Xs), empty_assoc(Ys) }.
input_format2(Xss, Yss) -->
    number(X),
    "   ",
    number(Y),
    eol,
    input_format2(Xs, Ys),
    { get_assoc(X, Xs, N0), N1 #= N0 + 1, put_assoc(X, Xs, N1, Xss);
      put_assoc(X, Xs, 1, Xss)
    },
    { get_assoc(Y, Ys, N0), N1 #= N0 + 1, put_assoc(Y, Ys, N1, Yss);
      put_assoc(Y, Ys, 1, Yss)
    }.

similarity(Assoc, K-V, Z) :-
    get_assoc(K, Assoc, Reps),
    Z #= K * V * Reps.
similarity(_, _, 0).

part2(F, Solution):-
    phrase_from_file(input_format2(Xs, Ys), F),
    assoc_to_list(Xs, Xss),
    maplist(similarity(Ys), Xss, Similarities),
    sum_list(Similarities, Solution).

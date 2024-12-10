:- use_module(dcgs/dcgs_utils).
:- use_module(fp).

:- dynamic(ok/1).

/*
Part 1:
  Instead of building up the number with all combinations, we destructure it
  using the right-most number:
   - we subtract if the result is #> 0, otherwise it would be impossible to
     construct with integer addition.
   - we divide if the mod is 0, otherwise it would be impossible to construct
     with integer multiplication.

Part 2:
  We also try to unconcat the string representation if the length would remain #> 0.
*/

real("07.txt").
sample("07.sample").

expr([]) --> "\n".
expr([X|Xs]) --> " ", number(X), expr(Xs).
expr([X|Xs]) --> number(X), ":", expr(Xs).

input([]) --> call(eos).
input([X|Xs]) --> expr(X), input(Xs).

ok(bogus).

is_ok(P, [X|Xs]) :-
    ok([X|Xs]);

    reverse(Xs, Ys),
    is_ok_helper(P, X, Ys),
    assertz(ok([X|Xs])).

is_ok_helper(_, X, [X]).
is_ok_helper(P, Res, [X|Xs]) :-
    Z #= Res - X,
    Z #> 0,
    is_ok_helper(P, Z, Xs).
is_ok_helper(P, Res, [X|Xs]) :-
    0 #= Res rem X,
    Z #= Res // X,
    is_ok_helper(P, Z, Xs).
is_ok_helper(p2, Res, [X|Xs]) :-
    number_chars(Res, Resc),
    number_chars(X, Xc),
    length(Resc, LR),
    length(Xc, LX),
    LR #> LX,
    append(Zc, Xc, Resc),
    number_chars(Z, Zc),
    is_ok_helper(p2, Z, Xs).

head([X|_], X).

part1(Xs, Sol) :-
    filter(is_ok(p1), Xs, Xss),
    maplist(head, Xss, Xsss),
    sum_list(Xsss, Sol).

part2(Xs, Sol) :-
    filter(is_ok(p2), Xs, Xss),
    maplist(head, Xss, Xsss),
    sum_list(Xsss, Sol).

run :-
    time(
        (
            phrase_from_file(input(Xs), "07.txt"),
            part1(Xs, X),
            format("Task 1: ~w~n", [X]),
            part2(Xs, Y),
            format("Task 2: ~w~n", [Y])
        )
    ),
    halt.

:- initialization(run).

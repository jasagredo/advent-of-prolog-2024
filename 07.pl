:- use_module(dcgs/dcgs_utils).
:- use_module(fp).

:- dynamic(ok/1).

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

part1(F, Sol) :-
    phrase_from_file(input(Xs), F),
    filter(is_ok(p1), Xs, Xss),
    maplist(head, Xss, Xsss),
    sum_list(Xsss, Sol).

part2(F, Sol) :-
    phrase_from_file(input(Xs), F),
    filter(is_ok(p2), Xs, Xss),
    maplist(head, Xss, Xsss),
    sum_list(Xsss, Sol).

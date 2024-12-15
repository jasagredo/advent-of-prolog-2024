:- use_module(library(dcgs)).
:- use_module(library(assoc)).
:- use_module(library(between)).
:- use_module(library(clpz)).
:- use_module(library(pio)).
:- use_module(library(lambda)).
:- use_module(library(lists)).
:- use_module(dcgs/dcgs_utils).
:- use_module(sort).

number_neg(X) -->
    "-", number(Y), !, { X #= -Y }.
number_neg(X) -->
    number(X).

robot(r(X-Y, Vx-Vy)) -->
    "p=", number(X), ",", number(Y), " ",
    "v=", number_neg(Vx), ",", number_neg(Vy).

pad(X, ['\n'|X]).
input([X|Xs]) -->
    "\n", robot(X), !, input(Xs).
input([]) --> "\n", call(eos).

quadrant(Mx-My, r(X-Y, _), A-B-C-D, A1-B1-C1-D1) :-
    ( X #< Mx, Y #< My, A1 #= A + 1
    ; A1 #= A
    ),
    ( X #< Mx, Y #> My, B1 #= B + 1
    ; B1 #= B
    ),
    ( X #> Mx, Y #< My, C1 #= C + 1
    ; C1 #= C
    ),
    ( X #> Mx, Y #> My, D1 #= D + 1
    ; D1 #= D
    ).

solve(Sx-Sy, Mx-My, S, r(X-Y, Vx-Vy), A-B-C-D, A1-B1-C1-D1) :-
    X1 #= (X + Vx * S) mod Sx,
    Y1 #= (Y + Vy * S) mod Sy,
    quadrant(Mx-My, r(X1-Y1, Vx-Vy), A-B-C-D, A1-B1-C1-D1).

part1(F, Sol) :-
    phrase_from_file((call(pad), input(Robots)), F),
    Mx #= 101 // 2,
    My #= 103 // 2,
    foldl(solve(101-103, Mx-My, 100), Robots, 0-0-0-0, A-B-C-D),
    Sol #= A * B * C * D.


step(Sx-Sy, Robots0, Robots) :-
    maplist(\ (r(X-Y, Vx-Vy))^(r(X1-Y1, Vx-Vy))^(X1 #= (X + Vx) mod Sx,
                                                 Y1 #= (Y + Vy) mod Sy),
            Robots0,
            Robots).

steps(Sizes, N, Robots, Sol) :-
    step(Sizes, Robots, Robots1),
    foldl(quadrant(50-51), Robots1, 0-0-0-0, A1-B1-C1-D1),
    (
        N #> 7490,
        mergesort_by(\A^B^(A #> B), [A1,B1,C1,D1], [X, Y|_]),
        X #> Y + 100,
        Sol #= N,
        format("Steps: ~w~n", [N-A1-B1-C1-D1]),
        print_map(Sizes, 0-0, Robots1);
        format("Steps: ~w~n", [N]),
        N1 #= N + 1,
        steps(Sizes, N1, Robots1, Sol)
    ).

print_map(_-Sy, _-Sy, _) :- !.
print_map(Sx-Sy, Sx-Y, Robots) :-
    Y1 #= Y + 1, !,
    format("~n", []),
    print_map(Sx-Sy, 0-Y1, Robots).
print_map(Sx-Sy, X-Y, Robots) :-
    (member(r(X-Y, _), Robots), !,
     format(".", []);
     \+ member(r(X-Y, _), Robots),
     format(" ", [])
    ),
    X1 #= X + 1,
    print_map(Sx-Sy, X1-Y, Robots).


part2(F, Sol) :-
    phrase_from_file((call(pad), input(Robots)), F),
    findall(_, steps(101-103, 0, Robots, Sol), _).

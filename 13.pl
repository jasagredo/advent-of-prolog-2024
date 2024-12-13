:- use_module(library(simplex)).
:- use_module(library(dcgs)).
:- use_module(library(clpz)).
:- use_module(library(pio)).
:- use_module(library(debug)).
:- use_module(dcgs/dcgs_utils).

button(X-Y) -->
    "Button ", [_], ": X+", number(X), ", Y+", number(Y).
prize(X-Y) -->
    "Prize: X=", number(X), ", Y=", number(Y).
machine(Tx-Ty, Ax-Ay, Bx-By) -->
    button(Ax-Ay), "\n", button(Bx-By), "\n", prize(Tx-Ty).

input(_, Z, Z) --> "\n", call(eos).
input(P, Z0, Z) -->
    "\n\n",
    machine(Tx-Ty, Ax-Ay, Bx-By),
    {
        simplex(P, Tx-Ty, Ax-Ay, Bx-By, A-B),
        Z1 #= Z0 + 3 * A + B;
        Z1 #= Z0
    },
    input(P, Z1, Z).

pad(X, ['\n', '\n'|X]).

constraints(P, TargetX-TargetY, Ax-Ay, Bx-By) -->
    constraint(integral(x1)),
    constraint(integral(x2)),
    ( { P = p1 },
      constraint([x1] =< 100),
      constraint([x2] =< 100),
      { TargetX1 #= TargetX,
        TargetY1 #= TargetY
      }
    ;
      { P = p2,
        TargetX1 #= 10000000000000 + TargetX,
        TargetY1 #= 10000000000000 + TargetY
      }
    ),
    constraint([Ax*x1, Bx*x2] = TargetX1),
    constraint([Ay*x1, By*x2] = TargetY1).

simplex(P, TargetX-TargetY, Ax-Ay, Bx-By, A-B) :-
    gen_state(S0),
    constraints(P, TargetX-TargetY, Ax-Ay, Bx-By, S0, S1),
    minimize([3*x1, x2], S1, S),
    variable_value(S, x1, A),
    variable_value(S, x2, B).

part1(F, Sol) :-
    phrase_from_file((call(pad), input(p1, 0, Sol)), F).

part2(F, Sol) :-
    phrase_from_file((call(pad), input(p2, 0, Sol)), F).

run :-
    part1("13.txt", Sol1),
    part2("13.txt", Sol2),
    format("Task 1: ~w~nTask 2: ~w~n", [Sol1, Sol2]),
    halt.

:- initialization(run).

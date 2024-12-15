:- use_module(library(dcgs)).
:- use_module(library(clpz)).
:- use_module(library(assoc)).
:- use_module(library(pio)).
:- use_module(library(debug)).
:- use_module(library(dif)).
:- use_module(library(lists)).

eos([], []).

map(_, A0, A0) -->
    "\n\n".
map(_-Y, A0, A1) -->
    "\n",
    { Y1 #= Y + 1 },
    !,
    map(0-Y1, A0, A1).
map(X-Y, Init0-A0, Init2-A2) -->
    [Char],
    { put_assoc(X-Y, A0, Char, A1) },
    { Char = '@', Init1 = X-Y;
      Char \= '\n', Char \= '@', Init1 = Init0
    },
    { X1 #= X + 1 },
    !,
    map(X1-Y, Init1-A1, Init2-A2).

right(X-Y, X1-Y) :-
    X1 #= X + 1.
left(X-Y, X1-Y) :-
    X1 #= X - 1.
up(X-Y, X-Y1) :-
    Y1 #= Y - 1.
down(X-Y, X-Y1) :-
    Y1 #= Y + 1.

move(_, X-Y, Map0, Map0) :-
    get_assoc(X-Y, Map0, (.)).
move(Dir, X-Y, Map0, Map) :-
    get_assoc(X-Y, Map0, Char),
    dif(Char, '#'),
    call(Dir, X-Y, X1-Y1),
    move(Dir, X1-Y1, Map0, Map1),
    put_assoc(X-Y, Map1, '.', Map2),
    put_assoc(X1-Y1, Map2, Char, Map).

print_map(_-5, _) :- format("~n~n", []).
print_map(4-Y, Map) :-
    Y1 #= Y + 1,
    format("~n", []),
    print_map(0-Y1, Map).
print_map(X-Y, Map) :-
    get_assoc(X-Y, Map, Char),
    format("~w", [Char]),
    X1 #= X + 1,
    print_map(X1-Y, Map).

process(_-Map, Map) -->
    call(eos).
process(X, Y) -->
    "\n", process(X, Y).
process(X-Y-Map, FinalMap) -->
    [Char],
    {
        (Char = (>), move(right, X-Y, Map, Map1), X1 #= X + 1, Y1 #= Y)
        ;
        (Char = (^), move(up, X-Y, Map, Map1), X1 #= X, Y1 #= Y - 1)
        ;
        (Char = 'v', move(down, X-Y, Map, Map1), X1 #= X, Y1 #= Y + 1)
        ;
        (Char = (<), move(left, X-Y, Map, Map1), X1 #= X - 1, Y1 #= Y)
        ;
        (X1 #= X, Y1 #= Y, Map1 = Map)
    },
    { * print_map(0-0, Map1) },
    process(X1-Y1-Map1, FinalMap).

compute(C, X-Y-C, Z0, Z1) :-
    Z1 #= 100 * Y + X + Z0.
compute(C, _-Char, Z0, Z0) :-
    dif(Char, C).

part1(F, Sol) :-
    phrase_from_file((map(0-0, (0-0)-t, Init-Map), process(Init-Map, FinalMap)), F),
    assoc_to_list(FinalMap, L),
    foldl(compute('O'), L, 0, Sol).

map2(_, A0, A0) -->
    "\n\n".
map2(_-Y, A0, A1) -->
    "\n",
    { Y1 #= Y + 1 },
    !,
    map2(0-Y1, A0, A1).
map2(X-Y, Init0-A0, Init2-A3) -->
    [Char],
    { X1 #= X + 1 },
    { Char = (#), put_assoc(X-Y, A0, Char, A1), put_assoc(X1-Y, A1, Char, A2);
      Char = 'O', put_assoc(X-Y, A0, '[', A1), put_assoc(X1-Y, A1, ']', A2);
      Char = '.', put_assoc(X-Y, A0, '.', A1), put_assoc(X1-Y, A1, '.', A2);
      Char = '@', put_assoc(X-Y, A0, '@', A1), put_assoc(X1-Y, A1, '.', A2)
    },
    { Char = '@', Init1 = X-Y;
      Char \= '\n', Char \= '@', Init1 = Init0
    },
    { X2 #= X1 + 1 },
    !,
    map2(X2-Y, Init1-A2, Init2-A3).

move2(_, X-Y, Map0, Map0) :-
    get_assoc(X-Y, Map0, (.)).
move2(Dir, X-Y, Map0, Map) :-
    (Dir = right; Dir = left ),
    get_assoc(X-Y, Map0, Char),
    dif(Char, '#'),
    call(Dir, X-Y, X1-Y1),
    move2(Dir, X1-Y1, Map0, Map1),
    put_assoc(X-Y, Map1, '.', Map2),
    put_assoc(X1-Y1, Map2, Char, Map).
move2(Dir, X-Y, Map0, Map) :-
    ( Dir = up; Dir = down ),
    get_assoc(X-Y, Map0, Char),
    dif(Char, '#'),
    ( Char = ']',
      X2 #= X - 1,
      call(Dir, X-Y, X1-Y1),
      call(Dir, X2-Y, X3-Y3),
      move2(Dir, X1-Y1, Map0, Map1),
      move2(Dir, X3-Y3, Map1, Map2),
      !,
      put_assoc(X-Y, Map2, '.', Map3),
      put_assoc(X2-Y, Map3, '.', Map4),
      put_assoc(X1-Y1, Map4, Char, Map5),
      put_assoc(X3-Y3, Map5, '[', Map)
    ;
      Char = '[',
      X2 #= X + 1,
      call(Dir, X-Y, X1-Y1),
      call(Dir, X2-Y, X3-Y3),
      move2(Dir, X1-Y1, Map0, Map1),
      move2(Dir, X3-Y3, Map1, Map2),
      !,
      put_assoc(X-Y, Map2, '.', Map3),
      put_assoc(X2-Y, Map3, '.', Map4),
      put_assoc(X1-Y1, Map4, Char, Map5),
      put_assoc(X3-Y3, Map5, ']', Map)
    ;
      Char \= '[', Char \= ']',
      call(Dir, X-Y, X1-Y1),
      move2(Dir, X1-Y1, Map0, Map1),
      put_assoc(X-Y, Map1, '.', Map2),
      put_assoc(X1-Y1, Map2, Char, Map)
    ).

process2(_-Map, Map) -->
    call(eos).
process2(X, Y) -->
    "\n", process2(X, Y).
process2(X-Y-Map, FinalMap) -->
    [Char],
    {
        (Char = (>), move2(right, X-Y, Map, Map1), X1 #= X + 1, Y1 #= Y)
        ;
        (Char = (^), move2(up, X-Y, Map, Map1), X1 #= X, Y1 #= Y - 1)
        ;
        (Char = 'v', move2(down, X-Y, Map, Map1), X1 #= X, Y1 #= Y + 1)
        ;
        (Char = (<), move2(left, X-Y, Map, Map1), X1 #= X - 1, Y1 #= Y)
        ;
        (X1 #= X, Y1 #= Y, Map1 = Map)
    },
    process2(X1-Y1-Map1, FinalMap).

part2(F, Sol) :-
    phrase_from_file((map2(0-0, (0-0)-t, Init-Map), process2(Init-Map, FinalMap)), F),
    assoc_to_list(FinalMap, L),
    foldl(compute('['), L, 0, Sol).

run :-
    part1("15.txt", Sol1),
    format("Task 1: ~w~n", [Sol1]),
    part2("15.txt", Sol2),
    format("Task 2: ~w~n", [Sol2]),
    halt.

:- initialization(run).

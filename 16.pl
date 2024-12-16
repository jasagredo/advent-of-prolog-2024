:- use_module(library(dcgs)).
:- use_module(library(clpz)).
:- use_module(library(pio)).
:- use_module(library(lambda)).
:- use_module(library(debug)).
:- use_module(library(ordsets)).
:- use_module(library(lists)).
:- use_module(library(dif)).
:- use_module(library(assoc)).
:- use_module(heaps).

eos([], []).

input(_, A0, A0) -->
    call(eos).
input(_-Y, A0, A1) -->
    "\n",
    { Y1 #= Y + 1 },
    input(0-Y1, A0, A1).
input(X-Y, Init0-End0-A0, A) -->
    [Char],
    { dif(Char, '\n'),
      put_assoc(X-Y, A0, Char, A1),
      X1 #= X + 1
    },
    { Char = 'S', Init1 = X-Y;
      Char \= 'S', Init1 = Init0
    },
    { Char = 'E', End1 = X-Y;
      Char \= 'E', End1 = End0
    },
    input(X1-Y, Init1-End1-A1, A).

turn(east, north-south).
turn(west, north-south).
turn(north, east-west).
turn(south, east-west).

move(east, X-Y, X1-Y) :-
    X1 #= X + 1.
move(west, X-Y, X1-Y) :-
    X1 #= X - 1.
move(north, X-Y, X-Y1) :-
    Y1 #= Y - 1.
move(south, X-Y, X-Y1) :-
    Y1 #= Y + 1.

visit_and_queue(Parent, Current, Prio, Visited0, Queue0, Visited, Queue) :-
    (
        % If already visited
        get_assoc(Current, Visited0, Px-Parents0),
        (
            % With this same prio
            Px #= Prio,
            % add myself as parent
            append([Parent], Parents0, Parents),
            put_assoc(Current, Visited0, Px-Parents, Visited)
        ;
            Px #\= Prio,
            Visited = Visited0
        ),
        Queue = Queue0
    ;
        % If not visited already, visit it now
        \+ get_assoc(Current, Visited0, _),
        put_assoc(Current, Visited0, Prio-[Parent], Visited),
        % And queue it on the heap
        add_to_heap(Queue0, Prio, Current, Queue)
    ).

process(A, Visited, Queue, Visited-Prio) :-
    get_from_heap(Queue, Prio, X-Y-_, _),
    get_assoc(X-Y, A, 'E').
process(A, Visited0, Queue0, Sol) :-
    get_from_heap(Queue0, Prio, X-Y-Dir, Queue1),
    turn(Dir, Dir1-Dir2),
    Prio1 #= Prio + 1000,
    visit_and_queue(X-Y-Dir, X-Y-Dir1, Prio1, Visited0, Queue1, Visited1, Queue2),
    visit_and_queue(X-Y-Dir, X-Y-Dir2, Prio1, Visited1, Queue2, Visited2, Queue3),
    move(Dir, X-Y, X1-Y1),
    get_assoc(X1-Y1, A, Cell),
    ( Cell \= (#),
      Prio2 #= Prio + 1,
      visit_and_queue(X-Y-Dir, X1-Y1-Dir, Prio2, Visited2, Queue3, Visited3, Queue4)
    ;
      Cell = (#),
      Visited3 = Visited2,
      Queue4 = Queue3
    ),
    process(A, Visited3, Queue4, Sol).

part1(Init-A, Sol) :-
    empty_heap(Empty),
    add_to_heap(Empty, 0, Init-east, Queue),
    put_assoc(Init-east, t, 0-[], Visited),
    process(A, Visited, Queue, Sol).

part2_helper(_, Init, Init-_, A0, A1) :-
    ord_add_element(A0, Init, A1).
part2_helper(Visited, _, Current, A0, A0) :-
    \+ get_assoc(Current, Visited, _).
part2_helper(Visited, Init, Current, A0, A) :-
    Current = X-Y-_,
    ord_add_element(A0, X-Y, A1),
    get_assoc(Current, Visited, _-Parents),
    foldl(part2_helper(Visited, Init), Parents, A1, A).

part2(Visited, Init-End, Sol) :-
    foldl(\Dir^part2_helper(Visited, Init, End-Dir), [north, south, east, west], [], L),
    length(L, Sol).

run :-
    phrase_from_file(input(0-0, 0-0-t, Init-End-A), "16.txt"),
    part1(Init-A, Visited-Sol1),
    format("Task 1: ~w~n", [Sol1]),
    part2(Visited, Init-End, Sol2),
    format("Task 2: ~w~n", [Sol2]),
    halt.

:- initialization(run).

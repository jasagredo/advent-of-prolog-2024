:- use_module(library(dcgs)).
:- use_module(library(ordsets)).
:- use_module(library(pio)).
:- use_module(library(clpz)).
:- use_module(library(format)).
:- use_module(library(assoc)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(lambda)).
:- use_module(dcgs/dcgs_utils).
:- use_module(heaps).

pad(X, ['\n'|X]).

input(N, N, A, A) --> ... .
input(_, _, A, A) --> call(eos).
input(MaxN, N, A0, A) -->
    "\n",
    number(X), ",", number(Y),
    { ord_add_element(A0, X-Y, A1), N1 #= N + 1 },
    input(MaxN, N1, A1, A).

visit_and_queue(Max, Map, Parent, Prio, Dir, Visited0-Queue0, Visited-Queue) :-
    move(Dir, Parent, Current),
    Current = X-Y,
    % if it is a rock, or it was visited, or it is out of bounds, do nothing
    ( ( ord_memberchk(Current, Map)
      ; get_assoc(Current, Visited0, _)
      ; (\+ between(0, Max, X))
      ; (\+ between(0, Max, Y))
      )
    -> Visited = Visited0, Queue = Queue0
    ;  % otherwise queue it
       put_assoc(Current, Visited0, Prio-Parent, Visited),
       % And queue it on the heap
       add_to_heap(Queue0, Prio, Current, Queue)
    ).

move(east, X-Y, X1-Y) :-
    X1 #= X + 1.
move(west, X-Y, X1-Y) :-
    X1 #= X - 1.
move(north, X-Y, X-Y1) :-
    Y1 #= Y - 1.
move(south, X-Y, X-Y1) :-
    Y1 #= Y + 1.

process(Max, _, Visited, Queue, Visited-Prio) :-
    get_from_heap(Queue, Prio, X-Y, _),
    X #= Max,
    Y #= Max.
process(Max, A, Visited0, Queue0, Sol) :-
    get_from_heap(Queue0, Prio, X-Y, Queue1),
    Prio1 #= Prio + 1,
    foldl( visit_and_queue(Max, A, X-Y, Prio1)
           , [north, east, south, west]
           , Visited0-Queue1
           , Visited-Queue
         ),
    !,
    process(Max, A, Visited, Queue, Sol).


part1(F, Sol) :-
    phrase_from_file((call(pad), input(1024, 0, [], Map)), F),
    empty_heap(Empty),
    add_to_heap(Empty, 0, 0-0, Queue),
    put_assoc(0-0, t, 0-nil, Visited),
    process(70, Map, Visited, Queue, _-Sol).

consume(0, X-Y) -->
    "\n",
    number(X), ",", number(Y), ... .
consume(N, Sol) -->
    { N #> 0 },
    "\n",
    number(_), ",", number(_),
    { N1 #= N - 1 },
    consume(N1, Sol).

part2_helper(F, Sol, Sol, Sol2) :-
    phrase_from_file((call(pad), consume(Sol, Sol2)), F).
part2_helper(F, Min, Max, Sol) :-
    Mid #= (Min + Max) // 2,
    (phrase_from_file((call(pad), input(Mid, 0, [], Map)), F),
     empty_heap(Empty),
     add_to_heap(Empty, 0, 0-0, Queue),
     put_assoc(0-0, t, 0-nil, Visited),
     \+ process(70, Map, Visited, Queue, _),
     part2_helper(F, Min, Mid, Sol)
    ;
     Mid1 #= Mid + 1,
     part2_helper(F, Mid1, Max, Sol)
    ).

part2(F, Sol) :-
    part2_helper(F, 1024, 3451, Sol).


run :-
    part1("18.txt", Sol1),
    format("Task 1: ~w~n", [Sol1]),
    part2("18.txt", X-Y),
    format("Task 2: ~w,~w~n", [X,Y]),
    halt.

:- initialization(run).

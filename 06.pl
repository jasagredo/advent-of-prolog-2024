:- use_module(library(between)).
:- use_module(dcgs/dcgs_utils).

real("06.txt", 129).
sample("06.sample", 9).

matrix(_, _, A, A) --> call(eos).
matrix(_, Y, A0, A1) -->
    "\n",
    { Y1 #= Y + 1 },
    matrix(0, Y1, A0, A1).
matrix(X, Y, A0-Init0, Res) -->
    [Char],
    { Char = (#),
      put_assoc(X-Y, A0, Char, A1);
      dif(Char, (#)),
      A1 = A0
    },
    { Char = (^),
      Init1 = X-Y;
      dif(Char, (^)),
      Init1 = Init0
    },
    { X1 #= X + 1 },
    matrix(X1, Y, A1-Init1, Res).

nextDir(up, right).
nextDir(right, down).
nextDir(down, left).
nextDir(left, up).

nextInDirection(_, up, X0-Y0, Map, Positions, Obstacle) :-
    findall(Y1, (member(X0-Y1, Map), Y1 #< Y0), Obstacles),
    (
        list_max(Obstacles, ObstacleY),
        Last #= ObstacleY+1,
        numlist(Last, Y0, Pos),
        maplist(tuple(X0), Pos, Positions),
        Obstacle = X0-ObstacleY;

        Obstacles = [],
        numlist(0, Y0, Pos),
        Obstacle = none,
        maplist(tuple(X0), Pos, Positions)
    ).
nextInDirection(Max, down, X0-Y0, Map, Positions, Obstacle) :-
    findall(Y1, (member(X0-Y1, Map), Y1 #> Y0), Obstacles),
    (
        list_min(Obstacles, ObstacleY),
        Last #= ObstacleY-1,
        numlist(Y0, Last, Pos),
        maplist(tuple(X0), Pos, Positions0),
        reverse(Positions0, Positions),
        Obstacle = X0-ObstacleY;

        Obstacles = [],
        numlist(Y0, Max, Pos),
        maplist(tuple(X0), Pos, Positions0),
        reverse(Positions0, Positions),
        Obstacle = none
    ).
nextInDirection(Max, right, X0-Y0, Map, Positions, Obstacle) :-
    findall(X1, (member(X1-Y0, Map), X1 #> X0), Obstacles),
    (
        list_min(Obstacles, ObstacleX),
        Last #= ObstacleX-1,
        numlist(X0, Last, Pos),
        maplist(fliple(Y0), Pos, Positions0),
        reverse(Positions0, Positions),
        Obstacle = ObstacleX-Y0;

        Obstacles = [],
        numlist(X0, _, Pos),
        maplist(fliple(Y0), Pos, Positions0),
        reverse(Positions0, Positions),
        Obstacle = none
    ).
nextInDirection(_, left, X0-Y0, Map, Positions, Obstacle) :-
    findall(X1, (member(X1-Y0, Map), X1 #< X0), Obstacles),
    (
        list_max(Obstacles, ObstacleX),
        Last #= ObstacleX+1,
        numlist(Last, X0, Pos),
        maplist(fliple(Y0), Pos, Positions),
        Obstacle = ObstacleX-Y0;

        Obstacles = [],
        numlist(0, X0, Pos),
        maplist(fliple(Y0), Pos, Positions),
        Obstacle = none
    ).


move(Max, Dir, Map, (X-Y)-Visited, Sol) :-
    nextInDirection(Max, Dir, X-Y, Map, Positions, Obstacle),
    foldl(\( (Px-Py)^A0^A1^put_assoc(Px-Py, A0, 0, A1)), Positions, Visited, Visited1),
    (Obstacle = none,
     assoc_to_list(Visited1, Sol);

     dif(Obstacle, none),
     Positions = [Last|_],
     nextDir(Dir, Dir1),
     move(Max, Dir1, Map, Last-Visited1, Sol)
    ).

makesLoop(Max, Map, P) :-
    makesLoop(Max, up, Map, P).
makesLoop(Max, Dir, Map, (X-Y)-Obstacles) :-
    nextInDirection(Max, Dir, X-Y, Map, Positions, Obstacle),
    dif(Obstacle, none),
    Positions = [Last|_],
    ( get_assoc(Obstacle-Dir, Obstacles, bogus), !;
      put_assoc(Obstacle-Dir, Obstacles, bogus, Obstacles1),
      nextDir(Dir, Dir1),
      makesLoop(Max, Dir1, Map, Last-Obstacles1)
    ).

part1and2(Mode, Sol1, Sol2) :-
    call(Mode, F, Max),
    empty_assoc(A0),
    phrase_from_file(matrix(0, 0, A0-(0-0), A1-Init), F),
    assoc_to_keys(A1, A2),
    move(Max, up, A2, Init-A0, Pos),
    maplist(fst, Pos, Pos1),
    length(Pos, Sol1),
    !,
    findall(P, (dif(P, Init), member(P, Pos1), makesLoop(Max, [P|A2], Init-A0)), NewObstacles),
    length(NewObstacles, Sol2).

:- use_module(library(dcgs)).
:- use_module(library(clpz)).
:- use_module(library(pio)).
:- use_module(library(assoc)).
:- use_module(library(lists)).
:- use_module(library(dif)).

eos([], []).

%% matrix(+X, +Y, +CCbyLetter, -Res)
%
% CCbyLetter :: assoc Letter (NextIdx, assoc Idx (Boundary, Positions))
matrix(_, _, Res, Res) -->
    call(eos).

matrix(_, Y, ByLetter, Res) -->
    "\n",
    { Y1 #= Y + 1 },
    matrix(0, Y1, ByLetter, Res).
matrix(X, Y, ByLetter, Res) -->
    [Char],
    { dif(Char, '\n'),

      ( get_assoc(Char, ByLetter, NextIdx-Components), !,
        assoc_to_list(Components, AsList),
        (
            % If there exists a component of my same letter, that contains my
            % left cell, then I add myself to that component
            X #> 0,
            X1 #= X - 1,
            select(LeftComponent-(Boundary0-Vertices), AsList, AsList1),
            get_assoc(X1-Y, Vertices, _),
            put_assoc(X-Y, Vertices, Char, Vertices1),
            % No new components were added
            NextIdx1 #= NextIdx,
            % I increase the boundary of this component in 2 fences
            Boundary1 #= Boundary0 + 2,
            MyComponent = LeftComponent-(Boundary1-Vertices1)
        ;
            % If there is no component that contains the cell on my left, then I
            % create a new component
            empty_assoc(A0),
            put_assoc(X-Y, A0, Char, Vertices),
            NextIdx1 #= NextIdx + 1,
            AsList1 = AsList,
            MyComponent = NextIdx-(4-Vertices)
        ),

        MyComponent = MyIdx-(MyBoundary-MyVertices),

        % now in vertical
        (
            % If there exists a component (which is not my component already) of
            % my same letter, that contains my top cell, then I union both
            % components.
            Y #> 0,
            Y1 #= Y - 1,
            select(_-(OtherBoundary-Vertices2), AsList1, AsList2),
            get_assoc(X-Y1, Vertices2, _),

            /* vv assoc_union */
            assoc_to_list(Vertices2, L1),
            assoc_to_list(MyVertices, L2),
            append(L1, L2, L3),
            list_to_assoc(L3, Vertices3),
            /* ^^ assoc_union */

            % If I combine two component then the fence separating me and the
            % top cell disappears, so -2.
            CombinedBoundary #= OtherBoundary + MyBoundary - 2,
            append([MyIdx-(CombinedBoundary-Vertices3)], AsList2, AsList3)
        ;
            % If there is no component (different from mine) that contains my
            % top cell, I only need to take care with having added too many
            % boundaries. For that I check if my top cell is actually in my own
            % component already and if so I subtract 2.
            (
                Y #> 0,
                Y1 #= Y - 1,
                get_assoc(X-Y1, MyVertices, _),
                CombinedBoundary #= MyBoundary - 2
            ;
                CombinedBoundary #= MyBoundary
            ),
            append([MyIdx-(CombinedBoundary-MyVertices)], AsList1, AsList3)
        ),
        list_to_assoc(AsList3, Components1),
        % Update the global assoc
        put_assoc(Char, ByLetter, NextIdx1-Components1, ByLetter1)
      ;
        % If there are no components for my letter, then I create the first one!
        empty_assoc(A0),
        put_assoc(X-Y, A0, Char, ThisCC),
        put_assoc(0, A0, 4-ThisCC, Components),
        put_assoc(Char, ByLetter, 1-Components, ByLetter1)
      ),
      XX #= X + 1
    },
    matrix(XX, Y, ByLetter1, Res).
matrix(Res) -->
    { empty_assoc(A0) },
    matrix(0, 0, A0, Res).


% Part 2

%  XYY
%  XYY
%  XXX
%
% The center cell is an outwards corner
outwards_corner(Vertices, (X1-Y1)-(X2-Y2), Score0, Score) :-
    \+ get_assoc(X1-Y1, Vertices, _),
    \+ get_assoc(X2-Y2, Vertices, _),!,
    Score #= Score0 + 1
    ;
    Score #= Score0.

%  YYY
%  YYY
%  XYY
%
% The center cell is an inwards corner
inwards_corner(Vertices, (X1-Y1)-(X2-Y2)-(X3-Y3), Score0, Score) :-
    get_assoc(X1-Y1, Vertices, _),
    get_assoc(X2-Y2, Vertices, _),
    \+ get_assoc(X3-Y3, Vertices, _), !,
    Score #= Score0 + 1
    ;
    Score #= Score0.

corner(Vertices, X-Y, Sol0, Sol) :-
    Xm #= X - 1,
    Xp #= X + 1,
    Ym #= Y - 1,
    Yp #= Y + 1,

    Up = X-Ym,
    Left = Xm-Y,
    Right = Xp-Y,
    Down = X-Yp,

    UpRight = Xp-Ym,
    UpLeft = Xm-Ym,
    DownLeft = Xm-Yp,
    DownRight = Xp-Yp,

    foldl(outwards_corner(Vertices), [Up-Left, Up-Right, Left-Down, Right-Down], Sol0, Sol1),
    foldl(inwards_corner(Vertices), [Up-Left-UpLeft, Up-Right-UpRight, Left-Down-DownLeft, Right-Down-DownRight], Sol1, Sol).

component(_-(Boundary-Vertices), X0-Y0, X1-Y1) :-
    assoc_to_keys(Vertices, L1),
    length(L1, Area),
    X1 #= X0 + Boundary * Area,
    foldl(corner(Vertices), L1, 0, Corners), !,
    Y1 #= Y0 + Area * Corners.

fold_components(G_3, _-(_-Components), Z0, Z1) :-
    fold_assoc(G_3, Components, Z0, Z1).

solve(F, Sol1, Sol2) :-
    phrase_from_file(matrix(ByLetters), F),
    fold_assoc(fold_components(component), ByLetters, 0-0, Sol1-Sol2).

fold_assoc(G_3, A, Z0, Z) :-
    assoc_to_list(A, L),
    foldl(G_3, L, Z0, Z).

run :-
    solve("12.txt", Sol1, Sol2),
    format("Task 1: ~w~nTask 2: ~w~n", [Sol1, Sol2]),
    halt.

:- initialization(run).

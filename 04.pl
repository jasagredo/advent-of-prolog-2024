:- use_module(dcgs/dcgs_utils).

matrix(_, _, _, A, A) --> call(eos).
matrix(C, _, Y, A0, A1) -->
    "\n",
    { Y1 #= Y + 1 },
    matrix(C, 0, Y1, A0, A1).
matrix(C, X, Y, (A0, Xs), Res) -->
    [Char],
    { dif(Char, '\n'), put_assoc(X-Y, A0, Char, A1), X1 #= X + 1 },
    { if_(=(Char,C), Xs1 = [X-Y|Xs], Xs1 = Xs) },
    matrix(C, X1, Y, (A1, Xs1), Res).

xmas([PM, PA, PS], Mat, Acc0, Acc1) :-
    get_assoc(PM, Mat, M),
    get_assoc(PA, Mat, A),
    get_assoc(PS, Mat, S),
    if_(=([M,A,S], "MAS"),
        Acc1 #= Acc0 + 1,
        Acc1 #= Acc0);
    Acc1 #= Acc0.

right(G_4, [X1-Y1, X2-Y2, X3-Y3], Mat, Acc0, Acc1) :-
    X1p #= X1 + 1,
    X2p #= X2 + 2,
    X3p #= X3 + 3,
    call(G_4, [X1p-Y1, X2p-Y2, X3p-Y3], Mat, Acc0, Acc1).

left(G_4, [X1-Y1, X2-Y2, X3-Y3], Mat, Acc0, Acc1) :-
    X1p #= X1 - 1,
    X2p #= X2 - 2,
    X3p #= X3 - 3,
    call(G_4, [X1p-Y1, X2p-Y2, X3p-Y3], Mat, Acc0, Acc1).

up(G_4, [X1-Y1, X2-Y2, X3-Y3], Mat, Acc0, Acc1) :-
    Y1p #= Y1 - 1,
    Y2p #= Y2 - 2,
    Y3p #= Y3 - 3,
    call(G_4, [X1-Y1p, X2-Y2p, X3-Y3p], Mat, Acc0, Acc1).

down(G_4, [X1-Y1, X2-Y2, X3-Y3], Mat, Acc0, Acc1) :-
    Y1p #= Y1 + 1,
    Y2p #= Y2 + 2,
    Y3p #= Y3 + 3,
    call(G_4, [X1-Y1p, X2-Y2p, X3-Y3p], Mat, Acc0, Acc1).


xmas_all(Mat, X-Y, Acc0, Acc8) :-
    length(Positions, 3),
    maplist(=(X-Y), Positions),

    right(xmas, Positions, Mat, Acc0, Acc1),
    left(xmas, Positions, Mat, Acc1, Acc2),
    up(xmas, Positions, Mat, Acc2, Acc3),
    down(xmas, Positions, Mat, Acc3, Acc4),
    right(up(xmas), Positions, Mat, Acc4, Acc5),
    right(down(xmas), Positions, Mat, Acc5, Acc6),
    left(up(xmas), Positions, Mat, Acc6, Acc7),
    left(down(xmas), Positions, Mat, Acc7, Acc8).

part1(F, Sol) :-
    empty_assoc(Mat0),
    phrase_from_file(matrix('X', 0, 0, (Mat0, []), (Mat1, Xs)), F),
    foldl(xmas_all(Mat1), Xs, 0, Sol).

x_mas(Mat, X-Y, Acc0, Acc1) :-
    Xr #= X + 1,
    Xl #= X - 1,
    Yu #= Y + 1,
    Yd #= Y - 1,
    get_assoc(Xr-Yu, Mat, RU),
    get_assoc(Xr-Yd, Mat, RD),
    get_assoc(Xl-Yu, Mat, LU),
    get_assoc(Xl-Yd, Mat, LD),
    permutation([RU, LD], Mas1),
    permutation([RD, LU], Mas2),
    maplist(=("MS"), [Mas1, Mas2]),
    Acc1 #= Acc0 + 1;
    Acc1 #= Acc0.

part2(F, Sol) :-
    empty_assoc(Mat0),
    phrase_from_file(matrix('A', 0, 0, (Mat0, []), (Mat1, As)), F),
    foldl(x_mas(Mat1), As, 0, Sol).

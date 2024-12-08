:- use_module(dcgs/dcgs_utils).
:- use_module(fp).

matrix(_, _, A, A) --> call(eos).
matrix(_, Y, A0, A1) -->
    "\n",
    { Y1 #= Y + 1 },
    matrix(0, Y1, A0, A1).
matrix(X, Y, A0, Res) -->
    [Char],
    { dif(Char, (.)),
      (   get_assoc(Char, A0, Acc),
          append(Acc, [X-Y], Acc1),
          put_assoc(Char, A0, Acc1, A1);
          put_assoc(Char, A0, [X-Y], A1)
      );
      Char = (.),
      A1 = A0
    },
    { X1 #= X + 1 },
    matrix(X1, Y, A1, Res).

in_matrix(X-Y) :-
    between(0, 49, X),
    between(0, 49, Y).

genp(Dx-Dy, X-Y, []) :-
    Xx #= X + Dx,
    Yy #= Y + Dy,
    \+ in_matrix(Xx-Yy).
genp(Dx-Dy, X-Y, [Xx-Yy|Xs]) :-
    Xx #= X + Dx,
    Yy #= Y + Dy,
    in_matrix(Xx-Yy),
    genp(Dx-Dy, Xx-Yy, Xs).

genm(Dx-Dy, X-Y, []) :-
    Xx #= X - Dx,
    Yy #= Y - Dy,
    \+ in_matrix(Xx-Yy).
genm(Dx-Dy, X-Y, [Xx-Yy|Xs]) :-
    Xx #= X - Dx,
    Yy #= Y - Dy,
    in_matrix(Xx-Yy),
    genm(Dx-Dy, Xx-Yy, Xs).

gen_antinodes_helper(P, (X1-Y1)-(X2-Y2), Antinodes) :-
    Dx #= X2 - X1,
    Dy #= Y2 - Y1,
    genp(Dx-Dy, X2-Y2, L1),
    genm(Dx-Dy, X1-Y1, L2),
    (
        P = p1, take1(L1, Ll1), take1(L2, Ll2), append(Ll1, Ll2, Antinodes);
        P = p2, append([L1, [X1-Y1, X2-Y2], L2], Antinodes)
    ).

gen_antinodes(P, Antennas, Antinodes) :-
    findall((X1-Y1)-(X2-Y2), sublist(Antennas, [X1-Y1,X2-Y2]), Pairs),
    maplist(gen_antinodes_helper(P), Pairs, Antinodes0),
    append(Antinodes0, Antinodes).

part1(F, Sol) :-
    empty_assoc(A),
    phrase_from_file(matrix(0, 0, A, Mat), F),
    assoc_to_values(Mat, AntennasByFreq),
    maplist(gen_antinodes(p1), AntennasByFreq, AntinodesByFreq),
    append(AntinodesByFreq, Antinodes),
    list_to_ord_set(Antinodes, UniqueAntinodes),
    length(UniqueAntinodes, Sol).

part2(F, Sol) :-
    empty_assoc(A),
    phrase_from_file(matrix(0, 0, A, Mat), F),
    assoc_to_values(Mat, AntennasByFreq),
    maplist(gen_antinodes(p2), AntennasByFreq, AntinodesByFreq),
    append(AntinodesByFreq, Antinodes),
    list_to_ord_set(Antinodes, UniqueAntinodes),
    length(UniqueAntinodes, Sol).

:- use_module(dcgs/dcgs_utils).
:- use_module(fp).

/*
Part 1:
  We produce an expanded list, then move file bytes from the end filling the
  spaces, until we have 'Ctr' file bytes since the start.

  Then compute the checksum.

Part 2 (horrible brute-force):
  We produce a compressed list, then process it backwards trying to find a place
  to put the file. When putting the file we also put the remaining free space.

  If we can't put the file, we re-append it to the result from the recursive
  call.

  Finally we expand the file-system (see 'expand/2') and we compute the
  checksum.
*/

real("09.txt").
sample("09.sample").

% Produces an expanded list, i.e. 121 --> [0,mt,mt,1]
space(_, [], 0) --> eol.
space(ID, Res, Ctr) -->
    [Char],
    { number_chars(SpaceSize, [Char]),
      length(L, SpaceSize),
      maplist(=(mt), L)
    },
    file(ID, Res0, Ctr),
    { append(L, Res0, Res) }.

file(_, [], 0) --> eol.
file(ID, Res, Ctr) -->
    [Char],
    { number_chars(FileSize, [Char]),
      length(L, FileSize),
      maplist(=(ID), L)
    },
    { ID1 #= ID + 1 },
    space(ID1, Res0, Ctr0),
    { append(L, Res0, Res),
      Ctr #= Ctr0 + FileSize
    }.

% Produces a compressed list, i.e. 121 --> [1-0, 2-mt, 1-1]
space2(_, []) --> eol.
space2(ID, [SpaceSize-mt|Res]) -->
    [Char],
    { number_chars(SpaceSize, [Char]), SpaceSize #\= 0 },
    file2(ID, Res).
space2(ID, Res) -->
    [Char],
    { number_chars(SpaceSize, [Char]), SpaceSize #= 0 },
    file2(ID, Res).

file2(_, []) --> eol.
file2(ID, [FileSize-ID|Res]) -->
    [Char],
    { number_chars(FileSize, [Char]) },
    { ID1 #= ID + 1 },
    space2(ID1, Res).

% Expands a list
expand([], []).
expand([Sz-ID|FS], Expanded) :-
    length(L, Sz),
    maplist(=(ID), L),
    expand(FS, Expanded0),
    append(L, Expanded0, Expanded).

% Compute checksum of an expanded list
checksum(FinalFS, Sol) :-
    checksum(0, FinalFS, Sol).
checksum(_, [], 0).
checksum(Pos, [mt|Xs], Sol) :-
    Pos1 #= Pos + 1,
    checksum(Pos1, Xs, Sol).
checksum(Pos, [X|Xs], Sol) :-
    dif(X, mt),
    V #= Pos * X,
    Pos1 #= Pos + 1,
    checksum(Pos1, Xs, Sol1),
    Sol #= V + Sol1.

compact_by_byte(0, _, _, []).
compact_by_byte(N, [X|Xs], Ys, [X|Compacted]) :-
    dif(X, mt),
    N1 #= N - 1,
    compact_by_byte(N1, Xs, Ys, Compacted).
compact_by_byte(N, [mt|Xs], [Y|Ys], [Y|Compacted]) :-
    dif(Y, mt),
    N1 #= N - 1,
    compact_by_byte(N1, Xs, Ys, Compacted).
compact_by_byte(N, [mt|Xs], [mt|Ys], Compacted) :-
    compact_by_byte(N, [mt|Xs], Ys, Compacted).

part1(Mode, Sol) :-
    call(Mode, F),
    phrase_from_file(file(0, FileSystem, Ctr), F),
    reverse(FileSystem, Reversed),
    compact_by_byte(Ctr, FileSystem, Reversed, FinalFS),
    checksum(FinalFS, Sol).

compact_by_region([], []).
compact_by_region([SpaceSize-mt|FS], [SpaceSize-mt|Compacted]) :-
    compact_by_region(FS, Compacted).
compact_by_region([FileSize-ID|FS], Compacted) :-
    * format("Moving file ~w with size ~w~n", [ID, FileSize]),
    dif(FS, mt),
    SpaceSize #>= FileSize,
    (
        reverse(FS, FSR),
        % find hole from the front
        append([Pre, [SpaceSize-mt], Post], FSR),
        % hole found
        NewEmpty #= SpaceSize - FileSize,
        (
            % Store the file and put back any remaining space
            NewEmpty #= 0,
            append([Pre, [FileSize-ID], Post], FSR2);
            NewEmpty #\= 0,
            append([Pre, [FileSize-ID, NewEmpty-mt], Post], FSR2)
        ),
        reverse(FSR2, FS2),
        compact_by_region(FS2, Compacted0),
        Compacted = [FileSize-mt|Compacted0]
    ;
        % no hole found
        compact_by_region(FS, Compacted0),
        Compacted = [FileSize-ID|Compacted0]
    ).

part2(Mode, Sol) :-
    call(Mode, F),
    phrase_from_file(file2(0, FileSystem), F),
    reverse(FileSystem, FileSystemR),
    compact_by_region(FileSystemR, CompactedR),
    reverse(CompactedR, Compacted),
    expand(Compacted, Expanded),
    checksum(Expanded, Sol).

run :-
    time(
        (
            part1(real, X),
            format("Task 1: ~w~n", [X]),
            part2(real, Y),
            format("Task 2: ~w~n", [Y])
        )
    ),
    halt.

:- initialization(run).

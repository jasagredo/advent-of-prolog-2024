:- use_module(library(dcgs)).
:- use_module(library(clpz)).
:- use_module(library(lambda)).
:- use_module(library(lists)).
:- use_module(library(pio)).

:- dynamic(cando/1).
:- dynamic(cando/2).
:- dynamic(cantdo/1).

eos([], []).

word(X) -->
    seq(X),
    { \+ member('\n', X),
      \+ member(',', X),
      \+ member(' ', X)
    }.

towels([]), "\n" --> "\n\n".
towels([X|Xs]) -->
    ", ", word(X), towels(Xs), { assertz(cando(X)) }.

patterns([]) --> "\n", call(eos).
patterns([X|Xs]) -->
    "\n", word(X), patterns(Xs).

cando(Towels, Pattern, Z0, Z) :-
    cando_helper(Towels, Pattern)
    -> Z #= Z0 + 1
    ; Z #= Z0.

cando_helper(Towels, Pattern) :-
    \+ cantdo(Pattern), !,
    (
        % either it is cached
        cando(Pattern)
    ;
        (
            member(T, Towels),
            append(T, P1, Pattern),
            cando_helper(Towels, P1),
            assertz(cando(Pattern))
        )
    )
    ; (assertz(cantdo(Pattern)), false).

part1(F, Sol) :-
    phrase_from_file((call(\X^([',', ' '| X])^true), towels(Towels), patterns(Patterns)), F),
    foldl(cando(Towels), Patterns, 0, Sol).


cando2(Towels, Pattern, Z0, Z) :-
    cando_helper(Towels, Pattern, N)
    -> Z #= Z0 + N
    ; Z #= Z0.

cando_helper(_, [], 1).
cando_helper(Towels, Pattern, N) :-
    \+ cantdo(Pattern), !,
    (
        % either it is cached
        cando(Pattern, N)
    ;
        (
            % find all decompositions which
            findall(Ns,
                    (
                        % the front part of the pattern is a towel
                        member(T, Towels),
                        append(T, P1, Pattern),
                        % and we can construct the rest in Ns ways
                        %
                        % TODO understand: I thought `a -> b; c` was equivalent
                        % to `(a, !, b; c)` but it is not, this code:
                        %
                        %  ```
                        %  ((cando_helper(Towels, P1, Ns), !); false)
                        %  ```
                        %
                        % is not equivalent to what is here below:
                        (cando_helper(Towels, P1, Ns)
                        -> true
                        ; false
                        )
                    ),
                    Nss),
            sum_list(Nss, N),
            % cache
            assertz(cando(Pattern, N))
        )
    )
    ; (assertz(cantdo(Pattern)), false).


part2(F, Sol) :-
    phrase_from_file((call(\X^([',', ' '| X])^true), towels(Towels), patterns(Patterns)), F),
    foldl(cando2(Towels), Patterns, 0, Sol).

run :-
    part1("19.txt", Sol1),
    format("Task 1: ~w~n", [Sol1]),
    part2("19.txt", Sol2),
    format("Task 2: ~w~n", [Sol2]),
    halt.

:- initialization(run).

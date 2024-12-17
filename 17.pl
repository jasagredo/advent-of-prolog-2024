:- use_module(library(dcgs)).
:- use_module(library(pio)).
:- use_module(library(assoc)).
:- use_module(library(time)).
:- use_module(library(clpz)).
:- use_module(dcgs/dcgs_utils).

register(X) --> "Register ", [_], ": ", number(X).
program(_, A, A) --> "\n", call(eos).
program(I, A0, A2) -->
    ",", number(X),
    { put_assoc(I, A0, X, A1), I1 #= I + 1 },
    program(I1, A1, A2).

program_header, "," -->
    "Program: ".

input(A-B-C, Program) -->
    register(A), "\n",
    register(B), "\n",
    register(C), "\n\n",
    program_header, program(0, t, Program).

combo(_, 0, 0).
combo(_, 1, 1).
combo(_, 2, 2).
combo(_, 3, 3).
combo(A-_-_, 4, A).
combo(_-B-_, 5, B).
combo(_-_-C, 6, C).


run_program(state(Registers, PC, Program), [], Registers) :-
    \+ get_assoc(PC, Program, _).
run_program(state(A-B-C, PC, Program), Output, S1) :-
    get_assoc(PC, Program, OpCode),
    ( OpCode #= 0,
      PC1 #= PC + 1,
      get_assoc(PC1, Program, Operand),
      combo(A-B-C, Operand, Value),
      A1 #= A // (2 ^ Value),
      PC2 #= PC1 + 1,
      State1 = state(A1-B-C, PC2, Program),
      Output = Output0
    ;
      OpCode #= 1,
      PC1 #= PC + 1,
      get_assoc(PC1, Program, Operand),
      B1 #= B xor Operand,
      PC2 #= PC1 + 1,
      State1 = state(A-B1-C, PC2, Program),
      Output = Output0
    ;
      OpCode #= 2,
      PC1 #= PC + 1,
      get_assoc(PC1, Program, Operand),
      combo(A-B-C, Operand, Value),
      B1 #= Value mod 8,
      PC2 #= PC1 + 1,
      State1 = state(A-B1-C, PC2, Program),
      Output = Output0
    ;
      OpCode #= 3,
      ( A #= 0,
        PC2 #= PC + 2,
        State1 = state(A-B-C, PC2, Program),
        Output = Output0
      ;
        A #\= 0,
        PC1 #= PC + 1,
        get_assoc(PC1, Program, Operand),
        PC2 #= Operand,
        State1 = state(A-B-C, PC2, Program),
        Output = Output0
      )
    ;
      OpCode #= 4,
      PC2 #= PC + 2,
      B1 #= B xor C,
      State1 = state(A-B1-C, PC2, Program),
      Output = Output0
    ;
      OpCode #= 5,
      PC1 #= PC + 1,
      get_assoc(PC1, Program, Operand),
      combo(A-B-C, Operand, Value),
      Value1 #= Value mod 8,
      PC2 #= PC1 + 1,
      State1 = state(A-B-C, PC2, Program),
      Output = [Value1|Output0]
    ;
      OpCode #= 6,
      PC1 #= PC + 1,
      get_assoc(PC1, Program, Operand),
      combo(A-B-C, Operand, Value),
      B1 #= A // (2 ^ Value),
      PC2 #= PC1 + 1,
      State1 = state(A-B1-C, PC2, Program),
      Output = Output0
    ;
      OpCode #= 7,
      PC1 #= PC + 1,
      get_assoc(PC1, Program, Operand),
      combo(A-B-C, Operand, Value),
      C1 #= A // (2 ^ Value),
      PC2 #= PC1 + 1,
      State1 = state(A-B-C1, PC2, Program),
      Output = Output0
    ),
    run_program(State1, Output0, S1).

part1(State, Sol) :-
    run_program(State, Sol, _).

part2(state(_-B-C, _, Program), Sol) :-
    assoc_to_values(Program, Out),
    Sol #> 0,
    run_program(state(Sol-B-C, 0, Program), Out, _),
    labeling([ff, min(Sol), bisect], [Sol]).

run :-
    time(
        (
            phrase_from_file(input(A-B-C, Program), "17.txt"),
            part1(state(A-B-C, 0, Program), X),
            format("Task 1: ~w~n", [X]),
            part2(state(A-B-C, 0, Program), Y),
            format("Task 2: ~w~n", [Y])
        )
    ),
    halt.

:- initialization(run).

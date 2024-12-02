:- module(dcgs_utils, [lines//1, eager_seq_of//2, eol//0, eos/2, number//1]).

:- use_module(library(dcgs)).
:- use_module(library(charsio)).
:- use_module(library(lists)).

:- meta_predicate eager_seq_of(1, ?, ?, ?).

%% The Power of Prolog
lines([])           --> call(eos), !.
lines([Line|Lines]) --> line(Line), lines(Lines).

line([])     --> ( "\n" ; call(eos) ), !.
line([L|Ls]) --> [L], line(Ls).

eager_seq_of(G_1, [E|Es]) --> [E], { call(G_1, E) }, eager_seq_of(G_1, Es).
eager_seq_of(_, []) --> [].

eol --> ("\n"; call(eos)).
eos([], []).

number(X) --> number([], X).
number(X, Z) --> [C], { char_type(C, numeric) }, number([C|X], Z).
number(X, Z) --> { length(X, L), L #> 0, reverse(X, X1), number_chars(Z, X1) }.

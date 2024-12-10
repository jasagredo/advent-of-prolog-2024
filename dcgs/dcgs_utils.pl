:- module(dcgs_utils, [lines//1, eol//0, eos/2, number//1]).

:- use_module(library(dcgs)).
:- use_module(library(charsio)).
:- use_module(library(lists)).

:- meta_predicate eager_seq_of(1, ?, ?, ?).

%% The Power of Prolog
lines([])           --> call(eos), !.
lines([Line|Lines]) --> line(Line), lines(Lines).

line([])     --> ( "\n" ; call(eos) ), !.
line([L|Ls]) --> [L], line(Ls).

eol --> ("\r\n"; "\n"; call(eos)).
eos([], []).

%% Parses a number
number(X) --> number([], X).
number(X, Z) --> [C], { char_type(C, numeric) }, number([C|X], Z).
number(X, Z) --> { length(X, L), L #> 0, reverse(X, X1), number_chars(Z, X1) }.

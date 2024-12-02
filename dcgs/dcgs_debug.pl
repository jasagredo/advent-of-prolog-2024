:- module(dcgs_debug, [op(900, xfx, @), op(900, fx, @), (@)/3, (@)/4]).

:- op(900, xfx, @).
:- op(900, fx, @).

% Debugging DCGs as:
%
% ```
%  my_dcg -->
%    N @ another_dcg
% ```
%
% will print the input truncated to N on call and the output truncated to N on
% exit. Omit N for un-truncated traces.
@(G_0, In, Out) :-
    portray_clause(dcg:call(G_0, In)),
    phrase(G_0, In, Out),
    portray_clause(dcg:exit(G_0, Out)).

@(N, G_0, In, Out) :-
    list_prefix_of_length(N, In, In1),
    portray_clause(dcg:call(G_0, In1)),
    phrase(G_0, In, Out),
    list_prefix_of_length(N, Out, Out1),
    portray_clause(dcg:exit(G_0, Out1)).

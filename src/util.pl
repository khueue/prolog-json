/** <module> Common utility predicates.
 */

:- module(_,
    [
        chars_number/2,
        looks_like_list/1
    ]).

:- include(json(include/common)).

%%  chars_number(+Chars, -Number) is semidet.
%%  chars_number(-Chars, +Number) is semidet.
%
%   True if Number is the numerical representation of Chars, where Chars
%   is any valid sequence of characters for a number (integer, float,
%   hex, etc.).

chars_number(Chars, Number) :-
    core:nonvar(Chars),
    !,
    core:atom_chars(Atom, Chars),
    core:atom_number(Atom, Number).
chars_number(Chars, Number) :-
    core:nonvar(Number),
    !,
    core:atom_number(Atom, Number),
    core:atom_chars(Atom, Chars).
chars_number(_Chars, _Number) :-
    throw(
        json_error(
            instantiation,
            context(chars_number/2, _Message))).

%%  looks_like_list(+List) is semidet.
%
%   True if List looks like a list, meaning that only the outermost structure
%   is investigated, in constant time (no recursion to see if the list is
%   proper).

looks_like_list([]).
looks_like_list([_|_]).

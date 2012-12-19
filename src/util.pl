/** <module> Common utility predicates.
 */

:- module(_,
    [
        chars_number/2,
        looks_like_list/1
    ]).

:- include(json(include/common)).

chars_number(Chars, Number) :-
    nonvar(Chars),
    !,
    core:atom_chars(Atom, Chars),
    core:atom_number(Atom, Number).
chars_number(Chars, Number) :-
    % Assume nonvar(Number).
    core:atom_number(Atom, Number),
    core:atom_chars(Atom, Chars).

looks_like_list([]).
looks_like_list([_|_]).

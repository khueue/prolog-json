/** <module> Common utility predicates.
 */

:- module(_, [
        get_context_and_throw//1,
        throw_error/3,
        looks_like_list/1
    ]).

:- include(json(include/common)).

%%  get_context_and_throw(+Predicate) is det.
%
%   Throws a parse exception indicating that something went wrong
%   in Predicate, and includes some context (reads some input).
%
%   @see throw_error/3

get_context_and_throw(Predicate) -->
    get_context(Message),
    { throw_error(parse, Predicate, Message) }.

get_context(Message) -->
    read_max_n_chars(40, Chars),
    { core:atom_chars(Context, Chars) },
    { core:atomic_list_concat(['Near: "',Context,'"'], Message) }.

read_max_n_chars(0, Cutoff) -->
    !,
    [],
    { core:atom_chars(' [...]', Cutoff) }.
read_max_n_chars(N, [Char|Chars]) -->
    % N > 0,
    [Char],
    !,
    { N1 is N - 1 },
    read_max_n_chars(N1, Chars).
read_max_n_chars(_N, []) --> [].

%%  throw_error(+Type, +Predicate, +Message) is det.
%
%   Throws an exception of the form
%
%       json_error(
%           Type,
%           context(Predicate, Message))
%
%   indicating that an error of Type was encountered in Predicate, with
%   an optional Message.

throw_error(Type, Predicate, Message) :-
    throw(json_error(Type,context(Predicate,Message))).

%%  looks_like_list(+List) is semidet.
%
%   True if List looks like a list, meaning that only the outermost structure
%   is investigated, in constant time (no recursion to see if the list is
%   proper).

looks_like_list([]).
looks_like_list([_|_]).

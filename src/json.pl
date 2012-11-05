/** <module> JSON manipulation.
 *
 *  JSON document manipulation and conversion to-and-from bytes.
 *
 *  @see <http://jsonspec.org/>
 */

:- module(_,
    [
    ]).

:- include(json(include/common)).

%%  version(?Version) is semidet.
%
%   True if Version is a list representing the major, minor
%   and patch version numbers of this library.

version([1,0,0]).

doc_json(Doc, Json) :-
    core:atom_chars(Json, JsonChars),
    phrase(parse_object(Doc), JsonChars, _JsonCharsRest).

parse_object(Doc) -->
    white,
    ['{'],
    parse_members(Doc),
    !,
    ['}'],
    white.
parse_object(Doc) -->
    white,
    ['{'],
    white,
    ['}'],
    white.

parse_members([Key-Value|Pairs]) -->
    parse_pair(Key-Value),
    [','],
    !,
    parse_members(Pairs).
parse_members([Key-Value]) -->
    parse_pair(Key-Value).

parse_pair(Key-Value) -->
    white,
    parse_key(Key),
    white,
    [':'],
    white,
    parse_value(Value),
    white.

parse_key(Key) -->
    ['"'],
    parse_chars(Key),
    ['"'].

parse_value(Value) -->
    ['"'],
    !,
    parse_chars(Value),
    ['"'].
parse_value(Value) -->
    parse_integer(Value).

parse_integer(Integer) -->
    parse_integer_aux(Chars),
    { core:atom_chars(Atom, Chars) },
    { core:atom_number(Atom, Integer) }.

parse_integer_aux([Char|Chars]) -->
    [Char],
    { core:char_type(Char, digit) },
    !,
    parse_integer_aux(Chars).
parse_integer_aux([]) --> [].

parse_chars(Atom) -->
    parse_chars_aux(Chars),
    { core:atom_chars(Atom, Chars) }.

parse_chars_aux([Char|Chars]) -->
    [Char],
    { valid_char(Char) },
    !,
    parse_chars_aux(Chars).
parse_chars_aux([]) --> [].

valid_char(Char) :-
    \+ lists:memberchk(Char, ['"']).

white -->
    [Char],
    { core:char_type(Char, space) },
    !,
    white.
white --> [].

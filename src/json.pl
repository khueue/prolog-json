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
    ws,
    ['{'],
    parse_members(Doc),
    !,
    ['}'],
    ws.
parse_object([]) -->
    ws,
    ['{'],
    ws,
    ['}'],
    ws.

parse_members([Key-Value|Pairs]) -->
    parse_pair(Key-Value),
    [','],
    !,
    parse_members(Pairs).
parse_members([Key-Value]) -->
    parse_pair(Key-Value).

parse_pair(Key-Value) -->
    ws,
    parse_key(Key),
    ws,
    [':'],
    ws,
    parse_value(Value),
    ws.

parse_key(Key) -->
    ['"'],
    parse_chars(Key),
    ['"'].

parse_value(Value) -->
    ['"'],
    parse_chars(Value),
    ['"'],
    !.
parse_value(Value) -->
    parse_float(Float),
    { chars_number(FloatChars, Float) }, % 5 . 0 5
    parse_exp(ExpChars),
    { lists:append(FloatChars, ExpChars, Chars) },
    { chars_number(Chars, Value) },
    !.
parse_value(Value) -->
    parse_float(Value),
    !.
parse_value(Value) -->
    parse_integer(Value),
    !.
parse_value(Value) -->
    parse_symbol(Value),
    !.
parse_value(Value) -->
    parse_object(Value),
    !.
parse_value(Value) -->
    parse_array(Value),
    !.

parse_array(Array) -->
    ['['],
    ws,
    parse_values(Array),
    !,
    ws,
    [']'].
parse_array([]) -->
    ['['],
    ws,
    [']'].

parse_values([Value|Values]) -->
    parse_value(Value),
    ws,
    [','],
    !,
    ws,
    parse_values(Values).
parse_values([Value]) -->
    parse_value(Value).

parse_symbol(+true)  --> [t,r,u,e], !.
parse_symbol(+false) --> [f,a,l,s,e], !.
parse_symbol(+null)  --> [n,u,l,l], !.

parse_exp([E|Chars]) -->
    parse_e(E),
    parse_sign(Sign),
    parse_digits(Digits),
    { lists:append(Sign, Digits, Chars) },
    !.
parse_exp([E|Digits]) -->
    parse_e(E),
    parse_digits(Digits).

parse_e('e') --> ['e'], !.
parse_e('E') --> ['E'], !.

parse_sign(['+']) --> ['+'], !.
parse_sign(['-']) --> ['-'], !.
parse_sign([])    --> [], !.

parse_integer(Integer) -->
    parse_digits_for_integer(Digits),
    { chars_number(Digits, Integer) }.

parse_digits_for_integer([Digit|Digits]) -->
    parse_digit_nonzero(Digit),
    !,
    parse_digits(Digits).
parse_digits_for_integer([Digit]) -->
    parse_digit(Digit).

parse_float(Float) -->
    parse_digits_for_integer(Integer),
    ['.'],
    parse_digits(Fraction),
    { lists:append(Integer, ['.'|Fraction], Chars) },
    { chars_number(Chars, Float) }.

parse_digit_nonzero(Digit) -->
    parse_digit(Digit),
    { Digit \== '0' }.

parse_digits([Digit|Digits]) -->
    parse_digit(Digit),
    !,
    parse_digits(Digits).
parse_digits([]) --> [].

parse_digit(Digit) -->
    [Digit],
    { core:char_type(Digit, digit) }.

parse_chars(Atom) -->
    parse_chars_aux(Chars),
    { core:atom_chars(Atom, Chars) }.

parse_chars_aux([Char|Chars]) -->
    parse_char(Char),
    !,
    parse_chars_aux(Chars).
parse_chars_aux([]) --> [].

parse_char(Char) -->
    [Char],
    { valid_char(Char) }.

ws -->
    ws_char,
    !,
    ws.
ws --> [].

ws_char -->
    [Char],
    { core:char_type(Char, space) }.

valid_char(Char) :-
    \+ lists:memberchk(Char, ['"']).

chars_number(Chars, Number) :-
    nonvar(Chars),
    !,
    core:atom_chars(Atom, Chars),
    core:atom_number(Atom, Number).
chars_number(Chars, Number) :-
    % nonvar(Number),
    core:atom_number(Atom, Number),
    core:atom_chars(Atom, Chars).

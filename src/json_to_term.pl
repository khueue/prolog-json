/** <module> JSON-to-term conversion.
 *
 *  Parses an atom of a JSON object and converts it into a Prolog term.
 */

:- module(_,
    [
        parse_object//1
    ]).

:- include(json(include/common)).

parse_object(json(Doc)) -->
    ws,
    ['{'],
    parse_members(Doc),
    !,
    ['}'],
    ws.
parse_object(json([])) -->
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
    ['\\'],
    !,
    parse_escape_sequence(Char),
    parse_chars_aux(Chars).
parse_chars_aux([Char|Chars]) -->
    parse_char(Char),
    !,
    parse_chars_aux(Chars).
parse_chars_aux([]) --> [].

parse_escape_sequence(RealChar) -->
    [Char],
    { valid_escape_char(Char, RealChar) },
    !.
parse_escape_sequence(Char) -->
    parse_hex_sequence(Char).

parse_hex_sequence(Char) -->
    ['u',Hex1,Hex2,Hex3,Hex4],
    { core:atomic_list_concat(['0x',Hex1,Hex2,Hex3,Hex4], HexAtom) },
    { core:atom_number(HexAtom, Code) },
    { core:atom_codes(Char, [Code]) }.

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

valid_escape_char('"',  '"').
valid_escape_char('\\', '\\').
valid_escape_char('/',  '/').
valid_escape_char('b',  '\b').
valid_escape_char('f',  '\f').
valid_escape_char('n',  '\n').
valid_escape_char('r',  '\r').
valid_escape_char('t',  '\t').

valid_char(Char) :-
    Char \== '"'.

chars_number(Chars, Number) :-
    nonvar(Chars),
    !,
    core:atom_chars(Atom, Chars),
    core:atom_number(Atom, Number).
chars_number(Chars, Number) :-
    % Assume nonvar(Number).
    core:atom_number(Atom, Number),
    core:atom_chars(Atom, Chars).

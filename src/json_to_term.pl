/** <module> JSON-to-term conversion.
 *
 *  Parses a JSON object atom and converts it into a Prolog term.
 */

:- module(_,
    [
        json_to_term/2
    ]).

:- include(json(include/common)).

:- use_module(json(util), []).

json_to_term(Json, Term) :-
    core:atom_chars(Json, JsonChars),
    phrase(parse_object(Term), JsonChars).

%   parse_object(Term)
%
%   XXX

parse_object(Term) -->
    ws,
    ['{'],
    parse_object_or_throw(Term).

parse_object_or_throw(json(Members)) -->
    parse_members(Members),
    ['}'],
    ws,
    !.
parse_object_or_throw(json([])) -->
    ws,
    ['}'],
    ws,
    !.
parse_object_or_throw(_) -->
    get_context_and_throw(parse_object//1).

parse_members([Pair|Members]) -->
    parse_pair(Pair),
    [','],
    !,
    parse_members(Members).
parse_members([Pair]) -->
    parse_pair(Pair).

parse_pair(Key-Value) -->
    ws,
    parse_key(Key),
    ws,
    [':'],
    ws,
    parse_value(Value),
    ws.

parse_key(Key) -->
    parse_string(Key).

parse_value(Value) -->
    parse_string(Value),
    !.
parse_value(Value) -->
    parse_number(Value),
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

%   parse_string(Term)
%
%   XXX

parse_string(Value) -->
    ['"'],
    parse_string_or_throw(Value).

parse_string_or_throw(Value) -->
    parse_chars(Value),
    ['"'],
    !.
parse_string_or_throw(_) -->
    get_context_and_throw(parse_string//1).

get_context_and_throw(Predicate) -->
    get_context(Message),
    { throw(json_error(parse,context(Predicate,Message))) }.

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

%   parse_array(Term)
%
%   XXX

parse_array(Array) -->
    ['['],
    ws,
    parse_array_or_throw(Array).

parse_array_or_throw(Array) -->
    parse_values(Array),
    ws,
    [']'],
    !.
parse_array_or_throw([]) -->
    ws,
    [']'],
    !.
parse_array_or_throw(_) -->
    get_context_and_throw(parse_array//1).

parse_values([Value|Values]) -->
    parse_value(Value),
    ws,
    [','],
    !,
    ws,
    parse_values(Values).
parse_values([Value]) -->
    parse_value(Value).

%   parse_symbol(Term)
%
%   XXX

parse_symbol(+true)  --> [t,r,u,e], !.
parse_symbol(+false) --> [f,a,l,s,e], !.
parse_symbol(+null)  --> [n,u,l,l], !.

%   parse_number(Term)
%
%   XXX

parse_number(Number) -->
    parse_float(Number),
    !.
parse_number(Number) -->
    parse_integer(Number).

parse_float(Float) -->
    parse_optional_minus(Minus),
    parse_digits_for_integer(Integer),
    ['.'],
    parse_float_or_throw(Fraction, Exponent),
    { lists:append([Minus,Integer,['.'],Fraction,Exponent], Chars) },
    { core:number_chars(Float, Chars) }.

parse_float_or_throw(Fraction, Exponent) -->
    parse_digits(Fraction),
    parse_optional_exponent(Exponent),
    !.
parse_float_or_throw(_, _) -->
    get_context_and_throw(parse_float//1).

parse_optional_exponent(Chars) -->
    parse_e(E),
    parse_optional_sign(Sign),
    parse_digits(Digits),
    !,
    { lists:append([E,Sign,Digits], Chars) }.
parse_optional_exponent([]) --> [].

parse_e(['e']) --> ['e'], !.
parse_e(['E']) --> ['E'], !.

parse_optional_sign(['+']) --> ['+'], !.
parse_optional_sign(['-']) --> ['-'], !.
parse_optional_sign([])    --> [], !.

parse_integer(Integer) -->
    parse_optional_minus(Minus),
    parse_digits_for_integer(Digits),
    { lists:append([Minus,Digits], Chars) },
    { core:number_chars(Integer, Chars) }.

parse_optional_minus(['-']) --> ['-'], !.
parse_optional_minus([])    --> [], !.

parse_digits_for_integer([Digit|Digits]) -->
    parse_digit_nonzero(Digit),
    !,
    parse_optional_digits(Digits).
parse_digits_for_integer([Digit]) -->
    parse_digit(Digit).

parse_digit_nonzero(Digit) -->
    parse_digit(Digit),
    { Digit \== '0' }.

parse_optional_digits([Digit|Digits]) -->
    parse_digit(Digit),
    !,
    parse_optional_digits(Digits).
parse_optional_digits([]) --> [].

parse_digits([Digit|Digits]) -->
    parse_digit(Digit),
    parse_optional_digits(Digits).

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

/** <module> Term-to-JSON conversion.
 *
 *  Converts a Prolog term into a JSON atom.
 */

:- module(_,
    [
        term_to_json/2
    ]).

:- include(json(include/common)).

:- use_module(json(util), []).

term_to_json(Term, Json) :-
    phrase(parse_object(Term), JsonChars),
    core:atom_chars(Json, JsonChars).

parse_object(json([])) -->
    !,
    ['{'],
    ['}'].
parse_object(json(Members)) -->
    ['{'],
    parse_members(Members),
    ['}'].

parse_members([Pair]) -->
    !,
    parse_pair(Pair).
parse_members([Pair|Pairs]) -->
    parse_pair(Pair),
    [','],
    parse_members(Pairs).

parse_pair(Key-Value) -->
    parse_key(Key),
    [':'],
    parse_value(Value).

parse_key(Key) -->
    parse_atom(Key).

parse_value(Value) -->
    { util:looks_like_list(Value) }, % Must precede atom() check!
    !,
    parse_array(Value).
parse_value(json(Value)) -->
    !,
    parse_object(json(Value)).
parse_value(+Value) -->
    !,
    parse_symbol(Value).
parse_value(Value) -->
    { core:atom(Value) },
    !,
    parse_atom(Value).
parse_value(Value) -->
    { core:float(Value) },
    !,
    parse_float(Value).
parse_value(Value) -->
    { core:integer(Value) },
    !,
    parse_integer(Value).

parse_atom(Value) -->
    ['"'],
    { core:atom_chars(Value, Chars) },
    parse_string_chars(Chars),
    ['"'].

parse_string_chars([]) --> !, [].
parse_string_chars(Chars) -->
    parse_special_chars(Chars, RestChars),
    !,
    parse_string_chars(RestChars).
parse_string_chars([Char|Chars]) -->
    [Char],
    parse_string_chars(Chars).

parse_special_chars(['\\'|Chars], RestChars) -->
    ['\\'],
    !,
    parse_escape_sequence(Chars, RestChars).
parse_special_chars([Char|Chars], Chars) -->
    { single_special_char(Char, EscapedChar) },
    ['\\',EscapedChar].

single_special_char('"',  '"').
single_special_char('/',  '/').
single_special_char('\b', 'b').
single_special_char('\f', 'f').
single_special_char('\n', 'n').
single_special_char('\r', 'r').

parse_escape_sequence(['u',Hex1,Hex2,Hex3,Hex4|Chars], Chars) -->
    ['u',Hex1,Hex2,Hex3,Hex4],
    !.
parse_escape_sequence(Chars, Chars) -->
    ['\\'].

parse_float(Value) -->
    { util:chars_number(Chars, Value) },
    Chars.

parse_integer(Value) -->
    { util:chars_number(Chars, Value) },
    Chars.

parse_array([]) -->
    !,
    ['['],
    [']'].
parse_array(Values) -->
    ['['],
    parse_array_values(Values),
    [']'].

parse_array_values([Value]) -->
    !,
    parse_value(Value).
parse_array_values([Value|Values]) -->
    parse_value(Value),
    [','],
    parse_array_values(Values).

parse_symbol(true)  --> !, [t,r,u,e].
parse_symbol(false) --> !, [f,a,l,s,e].
parse_symbol(null)  --> !, [n,u,l,l].

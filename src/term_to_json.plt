:- include(json(include/common)).

:- begin_tests('term_to_json:term_to_json/2').

test('term->json, integer', [true(Got == Expected)]) :-
    Term =
        json([
            k01 - 0,
            k02 - 42,
            k03 - -42
        ]),
    Expected =
        '{"k01":0,"k02":42,"k03":-42}',
    term_to_json:term_to_json(Term, Got).

test('term->json, float', [true(Got == Expected)]) :-
    Term =
        json([
            k01 - 0.0,
            k02 - 5.05,
            k03 - -5.05
        ]),
    Expected =
        '{"k01":0.0,"k02":5.05,"k03":-5.05}',
    term_to_json:term_to_json(Term, Got).

test('term->json, float exp', [true(Got == Expected)]) :-
    Term =
        json([
            k01 - 5.05e123,
            k02 - -5.05E-123
        ]),
    Expected =
        '{"k01":5.05e+123,"k02":-5.05e-123}',
    term_to_json:term_to_json(Term, Got).

test('term->json, string simple', [true(Got == Expected)]) :-
    Term =
        json([
            k01 - '',
            k02 - 'åäö string'
        ]),
    Expected =
        '{"k01":"","k02":"åäö string"}',
    term_to_json:term_to_json(Term, Got).

test('term->json, string escapes, special', [true(Got == Expected)]) :-
    Term =
        json([
            k01 - ' " \\ / '
        ]),
    Expected =
        '{"k01":" \\" \\\\ \\/ "}',
    term_to_json:term_to_json(Term, Got).

test('term->json, string escapes, control', [true(Got == Expected)]) :-
    Term =
        json([
            k01 - ' \b \\b \f \\f \n \\n \r \\r '
        ]),
    Expected =
        '{"k01":" \\b \\\\b \\f \\\\f \\n \\\\n \\r \\\\r "}',
    term_to_json:term_to_json(Term, Got).

test('term->json, string escapes, hex', [true(Got == Expected)]) :-
    Term =
        json([
            k01 - ' \u00a3 \\u00a3 '
        ]),
    Expected =
        '{"k01":" £ \\u00a3 "}',
    term_to_json:term_to_json(Term, Got).

test('term->json, array', [true(Got == Expected)]) :-
    Term =
        json([
            k01 - [],
            k02 - [123, åäö_string, 5.05]
        ]),
    Expected =
        '{"k01":[],"k02":[123,"åäö_string",5.05]}',
    term_to_json:term_to_json(Term, Got).

test('term->json, symbol', [true(Got == Expected)]) :-
    Term =
        json([
            k01 - +true,
            k02 - +false,
            k03 - +null
        ]),
    Expected =
        '{"k01":true,"k02":false,"k03":null}',
    term_to_json:term_to_json(Term, Got).

test('term->json, object', [true(Got == Expected)]) :-
    Term =
        json([
            k01 - json([]),
            k02 - json([k01-123,k02-åäö_string,k03-5.05])
        ]),
    Expected =
        '{"k01":{},"k02":{"k01":123,"k02":"åäö_string","k03":5.05}}',
    term_to_json:term_to_json(Term, Got).

:- end_tests('term_to_json:term_to_json/2').

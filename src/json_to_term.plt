:- include(json(include/common)).

:- use_module(json(json_to_term), []).

:- begin_tests('json_to_term:json_to_term/2').

test('json->term, integer', [true(Got == Expected)]) :-
    Json =
        '
        {
            "k01" : 0,
            "k02" : 42,
            "k03" : -42
        }
        ',
    Expected =
        json([
            k01 - 0,
            k02 - 42,
            k03 - -42
        ]),
    json_to_term:json_to_term(Json, Got).

test('json->term, float', [true(Got == Expected)]) :-
    Json =
        '
        {
            "k01" : 0.0,
            "k02" : 5.05,
            "k03" : -5.05
        }
        ',
    Expected =
        json([
            k01 - 0.0,
            k02 - 5.05,
            k03 - -5.05
        ]),
    json_to_term:json_to_term(Json, Got).

test('json->term, float exp', [true(Got == Expected)]) :-
    Json =
        '
        {
            "k01" : 0.0,
            "k02" : 5.05E+123,
            "k03" : -5.05e-123
        }
        ',
    Expected =
        json([
            k01 - 0.0,
            k02 - 5.05e+123,
            k03 - -5.05e-123
        ]),
    json_to_term:json_to_term(Json, Got).

test('json->term, string simple', [true(Got == Expected)]) :-
    Json =
        '
        {
            "k01" : "",
            "k02" : "åäö_string"
        }
        ',
    Expected =
        json([
            k01 - '',
            k02 - åäö_string
        ]),
    json_to_term:json_to_term(Json, Got).

test('json->term, string escapes, special', [true(Got == Expected)]) :-
    Json =
        '
        {
            "k01" : " \\" \\\\ \\/ "
        }
        ',
    Expected =
        json([
            k01 - ' " \\ / '
        ]),
    json_to_term:json_to_term(Json, Got).

test('json->term, string escapes, control', [true(Got == Expected)]) :-
    Json =
        '
        {
            "k01" : " \\b \\f \\n \\r "
        }
        ',
    Expected =
        json([
            k01 - ' \b \f \n \r '
        ]),
    json_to_term:json_to_term(Json, Got).

test('json->term, string escapes, hex', [true(Got == Expected)]) :-
    Json =
        '
        {
            "k01" : " \\u00a3 "
        }
        ',
    Expected =
        json([
            k01 - ' £ '
        ]),
    json_to_term:json_to_term(Json, Got).

test('json->term, symbol', [true(Got == Expected)]) :-
    Json =
        '
        {
            "k01" : true,
            "k02" : false,
            "k03" : null
        }
        ',
    Expected =
        json([
            k01 - +true,
            k02 - +false,
            k03 - +null
        ]),
    json_to_term:json_to_term(Json, Got).

test('json->term, array', [true(Got == Expected)]) :-
    Json =
        '
        {
            "k01" : [],
            "k02" : ["åäö_string", 42, 5.05, {}, {"key":"val"}, [42, 5.05, true]]
        }
        ',
    Expected =
        json([
            k01 - [],
            k02 - [åäö_string, 42, 5.05, json([]), json([key-val]), [42,5.05,+true]]
        ]),
    json_to_term:json_to_term(Json, Got).

test('json->term, object', [true(Got == Expected)]) :-
    Json =
        '
        {
            "k01" : {},
            "k02" : { "k01":123, "k02":5.05, "k03":{"key":"val"}, "k04":[42,true] }
        }
        ',
    Expected =
        json([
            k01 - json([]),
            k02 - json([k01-123, k02-5.05, k03-json([key-val]), k04-[42,+true]])
        ]),
    json_to_term:json_to_term(Json, Got).

:- end_tests('json_to_term:json_to_term/2').

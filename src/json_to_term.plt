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

test('json->term, complex object', [true(Got == Expected)]) :-
    Json =
        '
        {
            "k01" : "",
            "k02" : " \\" \\\\ \\/ \\b \\f \\n \\r \\u00a3 ",
            "k03" : "åäö_string",
            "k04" : 42,
            "k05" : 5.05,
            "k06" : [],
            "k07" : ["åäö_string", 42, 5.05, {}, {"key":"val"}, [42, 5.05, true]],
            "k08" : true,
            "k09" : false,
            "k10" : null
        }
        ',
    Expected =
        json([
            k01 - '',
            k02 - ' " \\ / \b \f \n \r £ ',
            k03 - 'åäö_string',
            k04 - 42,
            k05 - 5.05,
            k06 - [],
            k07 - [åäö_string, 42, 5.05, json([]), json([key-val]), [42,5.05,+true]],
            k08 - +true,
            k09 - +false,
            k10 - +null
        ]),
    json_to_term:json_to_term(Json, Got).

:- end_tests('json_to_term:json_to_term/2').

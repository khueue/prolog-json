:- include(json(include/common)).

:- begin_tests('json:doc_bytes/2').

test('json -> term', [true(Got == Expected)]) :-
    Json =
    '
    {
        "k01" : "",
        "k02" : "åäö_string",
        "k03" : 42,
        "k04" : 5.05,
        "k05" : [],
        "k06" : ["åäö_string", 42, 5.05, {}, {"key":"val"}, [42, 5.05, true]],
        "k07" : true,
        "k08" : false,
        "k09" : null
    }
    ',
    Expected =
    json([
        k01 - '',
        k02 - åäö_string,
        k03 - 42,
        k04 - 5.05,
        k05 - [],
        k06 - [åäö_string, 42, 5.05, json([]), json([key-val]), [42,5.05,+true]],
        k07 - +true,
        k08 - +false,
        k09 - +null
    ]),
    json:doc_json(Got, Json).

test('term -> json', [true(Got == Expected)]) :-
    Expected =
    json([
        k01 - '',
        k02 - åäö_string,
        k03 - 42,
        k04 - 5.05,
        k05 - [],
        k06 - [åäö_string, 42, 5.05, json([]), json([key-val]), [42,5.05,+true]],
        k07 - +true,
        k08 - +false,
        k09 - +null,
        k10 - json([])
    ]),
    json:doc_json(Expected, MinifiedJson),
    json:doc_json(Got, MinifiedJson).

:- end_tests('json:doc_bytes/2').

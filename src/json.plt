:- include(json(include/common)).

:- begin_tests('json:doc_bytes/2').

test('complex doc back-and-forth', [true(Got == Expected)]) :-
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

:- end_tests('json:doc_bytes/2').

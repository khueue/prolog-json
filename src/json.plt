:- include(json(include/common)).

:- begin_tests('json:doc_bytes/2').

test('json -> term', [true(Got == Expected)]) :-
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
    json:doc_json(Got, Json).

test('term -> json', [true(Got == Expected)]) :-
    Expected =
        json([
            k01 - '',
            %k02 - ' " \\ / \b \f \n \r £ ',
            k03 - åäö_string,
            k04 - 42,
            k05 - 5.05,
            k06 - [],
            k07 - [åäö_string, 42, 5.05, json([]), json([key-val]), [42,5.05,+true]],
            k08 - +true,
            k09 - +false,
            k10 - +null,
            k11 - json([]),
            k12 - json([key - val, mysymbol - +null, myarray - [1,2,3,aoeu]])
        ]),
    json:doc_json(Expected, MinifiedJson),
    %nl, write(MinifiedJson), nl,
    json:doc_json(Got, MinifiedJson).

:- end_tests('json:doc_bytes/2').

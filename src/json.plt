:- include(json(include/common)).

:- begin_tests('json:doc_bytes/2').
/*
test('nonvar, nonvar') :-
    Doc =
    [
        hello - 256
    ],
    Bytes =
    [
        16,0,0,0, % Length of top doc.
        0x10, % Tag.
            104,101,108,108,111, 0, % Ename.
            0,1,0,0, % Int32 data.
        0 % End of top doc.
    ],
    json:doc_bytes(Doc, Bytes).

test('nonvar, var', [true(Got == Expected)]) :-
    Doc =
    [
        hello - 256
    ],
    Expected =
    [
        16,0,0,0, % Length of top doc.
        0x10, % Tag.
            104,101,108,108,111, 0, % Ename.
            0,1,0,0, % Int32 data.
        0 % End of top doc.
    ],
    json:doc_bytes(Doc, Got).

test('var, nonvar', [true(Got == Expected)]) :-
    Expected =
    [
        hello - 256
    ],
    Bytes =
    [
        16,0,0,0, % Length of top doc.
        0x10, % Tag.
            104,101,108,108,111, 0, % Ename.
            0,1,0,0, % Int32 data.
        0 % End of top doc.
    ],
    json:doc_bytes(Got, Bytes).

test('var, var', [throws(json_error(_Description, _EnvList))]) :-
    json:doc_bytes(_, _).

test('complex doc back-and-forth', [true(Got == Expected)]) :-
    Json =
    '
    {
        "k01" : "",
        "k02" : "åäö_string",
        "k03" : 42,
        "k04" : 5.05,
        "k05" : [],
        "k06" : ["åäö_string", 42, 5.05, {"key":"val"}, [42, 5.05]],
        "k07" : true,
        "k08" : false,
        "k09" : null
    }
    ',
    Expected =
    [
        k01 - '',
        k02 - åäö_string,
        k03 - 42,
        k04 - 5.05,
        k05 - [],
        k06 - [åäö_string, 42, 5.05, [key-val], [+true,+false,+null]],
        k07 - +true,
        k08 - +false,
        k09 - +null
    ],
    json:doc_json(Expected, Json),
    json:doc_json(Got, Json).

:- end_tests('json:doc_bytes/2').
*/

test('simple stuff', [true(Got == Expected)]) :-
    Json =
    '
    {
        "k01" : "åäö_string",
        "k02" : 42,
        "k03" : 5.05
    }
    ',
    Expected =
    [
        k01 - åäö_string,
        k02 - 42,
        k03 - 5.05
    ],
    json:doc_json(Expected, Json),
    json:doc_json(Got, Json).

:- end_tests('json:doc_bytes/2').

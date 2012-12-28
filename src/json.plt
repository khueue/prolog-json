:- include(json(include/common)).

:- begin_tests('json:term_json/2').

test('verify library version', [true(Got == Expected)]) :-
    Expected = [1,0,0],
    json:version(Got).

test('complex object, back and forth', [true(Got == Expected)]) :-
    Expected =
        json([
            k01 - '',
            k02 - ' " \\ / \b \f \n \r £ \u00a3 ',
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
    json:term_json(Expected, MinifiedJson),
    json:term_json(Got, MinifiedJson).

test('both arguments var', [
        throws(json_error(instantiation,context(term_json/2,_Message)))
    ]) :-
    json:term_json(_, _).

:- end_tests('json:term_json/2').

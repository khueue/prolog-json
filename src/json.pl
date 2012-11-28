/** <module> JSON manipulation.
 *
 *  JSON document manipulation and conversion to-and-from bytes.
 *
 *  @see <http://jsonspec.org/>
 */

:- module(_,
    [
        doc_json/2,
        version/1
    ]).

:- include(json(include/common)).

:- use_module(json_to_term, []).
:- use_module(term_to_json, []).

%%  version(?Version) is semidet.
%
%   True if Version is a list representing the major, minor
%   and patch version numbers of this library.

version([1,0,0]).

doc_json(Doc, Json) :-
    core:nonvar(Json),
    !,
    core:atom_chars(Json, JsonChars),
    phrase(json_to_term:parse_object(Doc), JsonChars).
doc_json(Doc, Json) :-
    core:nonvar(Doc),
    !,
    phrase(term_to_json:parse_object(Doc), JsonChars),
    core:atom_chars(Json, JsonChars).

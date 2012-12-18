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
    json_to_term:json_to_term(Json, Doc).
doc_json(Doc, Json) :-
    core:nonvar(Doc),
    !,
    term_to_json:term_to_json(Doc, Json).

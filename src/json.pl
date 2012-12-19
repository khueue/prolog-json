/** <module> JSON parser.
 *
 *  @see <http://jsonspec.org/>
 */

:- module(_,
    [
        term_json/2,
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

%%  term_json(+Term, -Json) is semidet.
%%  term_json(-Term, +Json) is semidet.
%
%   True if Term is the Prolog representation of the JSON-encoded object
%   atom in Json.

term_json(Term, Json) :-
    core:nonvar(Json),
    !,
    json_to_term:json_to_term(Json, Term).
term_json(Term, Json) :-
    core:nonvar(Term),
    !,
    term_to_json:term_to_json(Term, Json).

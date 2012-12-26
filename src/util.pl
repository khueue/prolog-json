/** <module> Common utility predicates.
 */

:- module(_,
    [
        looks_like_list/1
    ]).

:- include(json(include/common)).


%%  looks_like_list(+List) is semidet.
%
%   True if List looks like a list, meaning that only the outermost structure
%   is investigated, in constant time (no recursion to see if the list is
%   proper).

looks_like_list([]).
looks_like_list([_|_]).

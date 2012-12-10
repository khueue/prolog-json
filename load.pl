% Acts as an interface to the system. Configures load paths and provides
% predicates for initiating the system.

%   json_configure_globals is det.
%
%   Configures useful globals used throughout the session.

json_configure_globals :-
    % For optimized compiles, tests are by default ignored.
    set_test_options([load(always)]).
    % Try to make everything as UTF-8 as possible.
    % set_prolog_flag(encoding, utf8). % When using streams, global setting.
    % Hunting implicit dependencies is easier without autoload.
    % set_prolog_flag(autoload, false),
    % Displays how modules and such are located.
    % set_prolog_flag(verbose_file_search, true).

%   json_configure_load_paths is det.
%
%   Configures internal load paths in preparation of use_module calls.

json_configure_load_paths :-
    prolog_load_context(directory, Root), % Available only during compilation.
    json_configure_path(Root, 'src', json).

json_configure_path(PathPrefix, PathSuffix, Name) :-
    atomic_list_concat([PathPrefix,PathSuffix], '/', Path),
    asserta(user:file_search_path(Name, Path)).

% Set everything up.
:- json_configure_globals.
:- json_configure_load_paths.

% Simply loading this module claims to speed up phrase, maplist, etc.,
% but I haven't noticed much difference.
% :- use_module(library(apply_macros)).

:- include(json(include/common)).

json_load_project_modules :-
    use_module(library(pldoc), []), % Load first to enable comment processing.
    use_module(json(json), []).

json_load_project_tests :-
    plunit:load_test_files([]).

%%  json_test is det.
%
%   Loads everything and runs the test suite.

json_test :-
    json_load_project_modules,
    json_load_project_tests,
    json_run_test_suite.

json_run_test_suite :-
    core:format('~n% Run tests ...~n'),
    plunit:run_tests.

%%  json_cov is det.
%
%   Loads everything and runs the test suite with coverage analysis.

json_cov :-
    json_load_project_modules,
    json_load_project_tests,
    json_run_test_suite_with_coverage.

json_run_test_suite_with_coverage :-
    core:format('~n% Run tests ...~n'),
    plunit:show_coverage(plunit:run_tests).

%%  json_repl is det.
%
%   Loads everything and enters interactive mode.

json_repl :-
    json_load_project_modules,
    json_load_project_tests.

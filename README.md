# JSON Parser for Prolog

## Short Example

	```prolog
	$ swipl

	?- [load].
	% load compiled 0.05 sec, 1,665 clauses
	true.

	?- json_load_project_modules.
	% library(pldoc) compiled into pldoc 0.14 sec, 1,297 clauses
	% json(util) compiled into util 0.00 sec, 21 clauses
	% json_to_term compiled into json_to_term 0.01 sec, 129 clauses
	% term_to_json compiled into term_to_json 0.00 sec, 46 clauses
	% json(json) compiled into json 0.01 sec, 192 clauses
	true.

	?- json:term_json(json([hello-42]), Json).
	Json = '{"hello":42}'.

	?- json:term_json(Term, '{"hello":42}').
	Term = json([hello-42]).
	```

## Todo

 * Go through escaping once more
 * Add proper error handling

## License

Licensed under the MIT license which can be found in the file
`LICENSE` in the project root.

## Coding Guidelines

 * Use empty imports (use_module(mymodule, [])) in order to not
   pollute the namespace.
 * Always use module prefixes (mymodule:predicate(...)) in order to
   clarify where things are coming from.
 * Always use the "made-up" module prefix "core:" when calling
   built-in predicates. This is completely unnecessary, and doesn't even
   work in all cases, but I think it is a good idea as long as it doesn't
   cause any problems. This decision may need to be revised when
   compatibility between different Prologs is investigated.
 * Avoid the if-then-else construct. It just looks ugly.
 * Avoid disjunctions. They are ugly, and can be replaced by properly
   written helpers. Think: premises are "and", clauses are "or".
 * Use cuts where appropriate, and try to keep each cut on a line by
   itself unless its placement is obvious and consistent in each clause.
   PlUnit is excellent at pointing out when tests succeed but leave
   choice points.
 * Try to avoid spaces within lists and structures, but always use
   spaces between arguments.
 * Predicates, atoms, etc. should use "this_naming_style" while variables
   should use "ThisNamingStyle".
 * Try to stick to the PlDoc structure.
 * If in doubt, consult: <http://www.ai.uga.edu/mc/plcoding.pdf>

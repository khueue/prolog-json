# JSON Parser for Prolog

A JSON (<http://jsonspec.org/>) parser written in Prolog. Used by the
Prolog MongoDB driver prolongo, <https://github.com/khueue/prolongo>.

## Release History

### Version 1.0.0 (2012-09-10)

 * First release as a separate project (previously included in
<https://github.com/khueue/prolongo>).
 * Exceptions now follow: `json_error(DescriptionAtom, ListOfRelatedVars)`

## Todo

 * Nothing for now.

## Usage

Clone the repository, switch to a certain release if you like (`git
checkout v1.0.0`) and run `make` to compile the necessary C libraries
and run the test suite. See the tests (*.plt) in the src folder for usage
examples.

## Dependencies

 * SWI-Prolog (tested on Mac OS X using SWI 6.0.2)
 * ANSI C compiler (modify Makefile if not GCC) (tested on Mac OS X
   using Clang)

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

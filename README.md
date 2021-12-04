# MiniSchemeProlog
Toy Demo of a tiny Interpreter for Scheme in Prolog
## Implementation
MiniSchemeProlog is just a reference implementation of a simplified Scheme for teaching purposes, currently written in [SWI-Prolog](https://www.swi-prolog.org/).
The main purpose is simply to illustrate how easy is to write language prototypes in Prolog.

The `src/main/scheme.pl` is the interpreter. It assumes a simple AST representation of Scheme.
The `src/main/scope.pl` contains a mutable implementation of an scope.
Some small test examples can be found in `src/test/test.pl`.

For instance, the following test evaluates the AST of the SEXP `(if #t (* 10 20) (+ 10 20))`:

```prolog
    test1 :-
        scope_cleanall,
        scopeNew(Scope), 
        Sexp = if(bool('#t'), sexp([id(*), num(10), num(20)]), 
	                      sexp([id(+), num(10), num(20)])),
        evaluate(Sexp, Scope, Result),!,
        format('~n~q~n', [Result]),
        scopeDump(Scope)
.
````

The `evaluate(+Sexp, +Scope, -Result)` predicate is the evaluator. 
A small spec describing the ASTs can be found in `doc/spec.txt`
## TO DO
1. Soon, we will be adding a parser using DCG.
2. Adding modularization 
3. Unit testing
4. PlDoc the code

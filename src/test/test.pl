/*
* Test of MiniScheme in Prolog
* @author loriacarlos@gmail.com
* @since 2021
*/
:-['../main/scheme'].
%%%%%%%%%%%%%%%%%%%%%%%%%% Test Cases %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test1 :-
    scope_cleanall,
    scopeNew(Scope), 
    Sexp = if(bool('#t'), sexp([id(*), num(10), num(20)]), 
	                      sexp([id(+), num(10), num(20)])),
	evaluate(Sexp, Scope, Result),!,
	format('~n~q~n', [Result]),
	scopeDump(Scope)
.

test2 :-
    scope_cleanall,
    scopeNew(Scope), 
    Sexp = begin([
	              display(str('*** test 2 ***')),
				  newline,
	              display(sexp([id(*), num(10), num(20)])),
				  newline
				 ]),
	evaluate(Sexp, Scope, Result),!,
	format('~n~q~n', [Result]),
	scopeDump(Scope)
.

test3 :-
    scope_cleanall,
    scopeNew(Scope), 
    Sexp = begin([
	         display(str('*** test 3 ***\n')),
			 define(id(x), num(10)),
			 display(id(x)),
			 newline,
			 define(id(y), num(20)),
	         define(id(result), 
			        if(bool('#t'), sexp([id(*), id(x), id(y)]), 
	                               sexp([id(+), id(x), id(y)]))),
			 display(id(result)),
			 newline
		   ]),
	evaluate(Sexp, Scope, Result),!,
	format('~n~q~n', [Result]),
	scopeDump(Scope)
.
test4 :-
    scope_cleanall,
    scopeNew(Scope), 
    Sexp = progn([
	         display(str('*** test 4 ***\n')),
			 define(id(x), num(10)),
			 display(id(x)),
			 newline,
			 define(id(y), num(20)),
	         define(id(result), 
			        if(bool('#t'), sexp([id(*), id(x), id(y)]), 
	                               sexp([id(+), id(x), id(y)]))),
			 display(id(result)),
			 newline
		   ]),
	evaluate(Sexp, Scope, Result),!,
	format('~n~q~n', [Result]),
	scopeDump(Scope)
.

test5 :-
    scope_cleanall,
    scopeNew(Scope), 
    Sexp = progn([
	         display(str('*** test 5 ***\n')),
			 define(id(x), list([num(1), bool('#t'), str('hello')])),
			 display(quote(id(x))),
			 newline,
			 display(id(x))
		   ]),
	evaluate(Sexp, Scope, Result),!,
	format('~n~q~n', [Result]),
	scopeDump(Scope)
.				
																

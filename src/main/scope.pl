/*
* Simple Scope in Prolog
* @author loriacarlos@gmail.com
* @since 2021
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%% SCOPE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
scope_prefix('$scope_').
scopeNew(Id, Parent) :- scope_prefix(P),
                        gensym(P, Id),
                        assert(scope(Id, Parent))				
.
scopeNew(Id)   :- scopeNew(Id, null),
                forall(member(P, [+, -, *, /, cons, car, cdr]),
				       assert(scope_local(Id, id(P) -> prim(P))))
.
scope_cleanall :- scope_prefix(P),
                  reset_gensym(P),
                  retractall(scope(_,_)),
				  retractall(scope_local(_,_))
.
scopePushLocalBinding(Scope, X -> V) :- retract( scope_local(Scope, X -> _) ), !,
                                        assert( scope_local(Scope, X -> V) ).
scopePushLocalBinding(Scope, X -> V) :- assert( scope_local(Scope, X -> V) ).

scopeFetch(Scope, X -> V) :- scope_local(Scope, X -> V), !.
scopeFetch(Scope, X -> V) :- scope(Scope, Parent), scopeFetch(Parent, X -> V), !.
scopeFetch(Scope, X -> V) :- throw(scopeFetch(Scope, X -> V)).


scopeReplaceBinding(Scope, X -> V) :- retractall(scope_local(Scope, X -> _)),
                                      assert(scope_local(Scope, X -> V))
.

scopeDump(Scope) :-
    writeln(Scope),
    forall(scope_local(Scope, B), writeln(B)),
	scope(Scope, Parent),
	(Parent \= null -> scopeDump(Parent); true)
.

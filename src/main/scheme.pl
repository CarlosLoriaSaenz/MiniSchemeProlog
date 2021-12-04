/*
* MiniScheme in Prolog
* @author loriacarlos@gmail.com
* @since 2021
*/
:-[scope].
%%%%%%%%%%%%%%%%%%%%%%%%%%%% Evaluator %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Quote
evaluate( quote(Expr), _Scope, Expr ) :- !.
% Atomic
evaluate( id(X), Scope, V) :- !, scopeFetch(Scope, id(X) -> V).
evaluate( num(N), _Scope, N ):- !. 
evaluate( str(S), _Scope, S ):- !. 
evaluate( bool('#t'), _Scope, true ) :- !.
evaluate( bool('#f'), _Scope, false ):- !.
evaluate( nil, _Scope, _Result ) :- !, throw('Empty (nil) Procedure cannot be evaluated').
% Lambda (Unary only)
evaluate( lambda(id(X), Body), LexicalScope, closure(id(X), Body, LexicalScope) ) :- !.
% Sexp apply
evaluate( sexp([OperExpr | ArgsList]), Scope, Result ) :- !,
    evaluate( OperExpr, Scope, Oper ),
	evaluateAll( ArgsList, Scope, ResultsArgsList ),
	applyOper(Oper, ResultsArgsList, Result)
.
evaluate( sexp([]), Scope, Result ) :- !, evaluate(nil, Scope, Result).
% Define, if
evaluate( define(id(X), Value), Scope, X ) :- !,
    Id = id(X),
    scopeReplaceBinding( Scope, Id -> nil ),
	evaluate( Value, Scope, EvalValue ),
	scopeReplaceBinding( Scope, Id -> EvalValue)
.
evaluate( if(Cond, Then, Else), Scope, Result) :- !,
   evaluate(Cond, Scope, true) -> evaluate(Then, Scope, Result) 
                               ;  evaluate(Else, Scope, Result)
.
% IO: display, newline  
evaluate( display(Expr), Scope, nil) :-
  evaluate( Expr, Scope, ExprValue ),
  format('~w', [ExprValue] )
.
evaluate( newline, _Scope, nil ) :- nl.
% Blocks: prog1, progn, begin, list
evaluate( prog1(ArgsList), Scope, Result) :-
   evaluateAll(ArgsList, Scope, ResultList),
   (ResultList == [] -> Result=nil ; [Result | _] = ResultList )   
.
evaluate( progn(ArgsList), Scope, Result) :-
   evaluateAll(ArgsList, Scope, ResultList),
   (ResultList == [] -> Result=nil ; last(ResultList, Result) )   
.
evaluate( begin(ArgsList), Scope, Result) :-
   scopeNew(BeginScope, Scope),
   evaluate(progn(ArgsList), BeginScope, Result)   
.
evaluate( list(ArgsList), Scope, Result) :-
   evaluateAll(ArgsList, Scope, Result)
.
evaluate( UnExpectedAst, _Scope, _ ) :- throw(evaluate(UnExpectedAst)).

%%%%%%%%%%%%%%%%%%%%%%%%%% Auxiliary evaluateAll and applyOper %%%%%%%%%%%%%%%%%%%%%%%
evaluateAll([], _Scope, []).
evaluateAll([Expr | Rest], Scope, [ExprValue | RestValue] ) :-
   evaluate( Expr, Scope, ExprValue ),
   evaluateAll( Rest, Scope, RestValue )
. 
applyOper(closure(id(X), Body, LexicalScope), ArgsList, Result) :- !,
    scopeNew(ClosureScope, LexicalScope),
    ArgsList = [ActualParam], !,
    scopeReplaceBinding(ClosureScope, id(X) -> ActualParam),
    evaluate(Body, ClosureScope, Result )
	
.
applyOper(prim(Oper), ArgsList, Result ) :- !,
         arithPrimitive(Oper), !,
		 setDefaultWhenEmpty(Oper, ArgsList, NotEmptyArgsList),
         Expr =.. [Oper | NotEmptyArgsList], 
		 Result is Expr
.
applyOper(car, [Car | _], Car ) :- !.
applyOper(cdr, [_ | Cdr], Cdr ) :- !. 
applyOper(cons,[First, List], [First | List]) :- !.
applyOper(list, List, List ) :- is_list(List), !.
applyOper(Oper, List, _ ) :- throw(applyOper(Oper, List)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Utils %%%%%%%%%%%%%%%%%%%%%%%%
arithPrimitive(Oper) :- member(Oper, [+, -, *, /]), !.
setDefaultWhenEmpty(+, Args, NotEmptyArgs) :- !, Args==[] -> NotEmptyArgs = [0]; NotEmptyArgs = Args.
setDefaultWhenEmpty(*, Args, NotEmptyArgs) :- !, Args==[] -> NotEmptyArgs = [1]; NotEmptyArgs = Args.
setDefaultWhenEmpty(Oper, Args, NotEmptyArgs) :-    Args==[] -> throw(setDefaultWhenEmpty(Oper, []));
                                                                NotEmptyArgs= Args
.
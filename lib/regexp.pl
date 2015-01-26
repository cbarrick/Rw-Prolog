:- use_module(library(dcg/basics)).
:- use_module('../src/rewrite').

:- op(550, xfy, (::)).

%! regexp(+Expression)
% Terms of this form represent regular expressions. `Expression` is the regexp
% as a string or code list.
%
% Users are expected to write terms in this form, but the methods are defined
% on compiled terms of the form `regexp(Expression, NFA)`. This rewrite rule
% allows the user form to be rewritten into the compiled form.

regexp(Expression) := regexp(Expression, NFA) :-
	phrase(regexp_phrase(Tree), Expression, []),
	compile(Tree, NFA, start, accept).


%! regexp(+Expression, +NFA)::match(+Input)
% Terms of this form simplify to `true` when the input string is matched by
% the regular expression, i.e. the NFA reached its accept state.

% Expand to the form `regexp(Exp, NFA)::match(Input, CurrentState)`
regexp(Exp, NFA)::match(Input) := regexp(Exp, NFA)::match(Input, start).

% We're done whenever we're in an accept state
regexp(_, _)::match(_, accept) := true :- !.

% There are 3 possible transition types -- Either consume a code from the input
% using a coresponding transition, consume a code using a wildcard transition,
% or consume nothing using a null rull.
regexp(Exp, NFA)::match([H|T], State) := regexp(Exp, NFA)::match(T, Next) :-
	member(edge(State,Next,H), NFA).
regexp(Exp, NFA)::match([_|T], State) := regexp(Exp, NFA)::match(T, Next) :-
	member(edge(State,Next,wild), NFA).
regexp(Exp, NFA)::match(Input, State) := regexp(Exp, NFA)::match(Input, Next) :-
	member(edge(State,Next,null), NFA).

regexp(_,_)::match(_,_) := _ :- !, fail.


% Compiler
% -------------------------

compile(literal(Char), [edge(Start,Accept,Char)], Start, Accept).

compile(wild, [edge(Start,Accept,wild)], Start, Accept).

compile(or(X,Y), NFA, Start, Accept) :-
	compile(X, NFA_X, Start, Accept),
	compile(Y, NFA_Y, Start, Accept),
	append(NFA_X, NFA_Y, NFA).

compile(and(X,Y), NFA, Start, Accept) :-
	gensym(state, Q1),
	compile(X, NFA_X, Start, Q1),
	compile(Y, NFA_Y, Q1, Accept),
	append(NFA_X, NFA_Y, NFA).

compile(maybe(X), NFA, Start, Accept) :-
	compile(X, NFA_X, Start, Accept),
	NFA = [edge(Start,Accept,null)|NFA_X].

compile(star(X), NFA, Start, Accept) :-
	compile(X, NFA_X, Start, Start),
	NFA = [edge(Start,Accept,null)|NFA_X].


% Parser
% -------------------------

%! regexp_phrase(-Tree)// is det
% Parses regular expressions
regexp_phrase(X) -->
	regexp_phrase(0, X),
	!.

% Precedence 0 (highest): Alternation
regexp_phrase(0, or(X,Y)) -->
	regexp_phrase(1, X),
	"|",
	regexp_phrase(0, Y).

% Precedence 1: Concatenation
regexp_phrase(1, and(X,Y)) -->
	regexp_phrase(2, X),
	regexp_phrase(1, Y).

% Precedence 2: Anchors
% Not implemented

% Precedence 3: Quantifiers
regexp_phrase(3, maybe(X)) -->
	regexp_phrase(4, X),
	"?".
regexp_phrase(3, star(X)) -->
	regexp_phrase(4, X),
	"*".
regexp_phrase(3, and(X,star(X))) -->
	regexp_phrase(4, X),
	"+".

% Precedence 4: Grouping
regexp_phrase(4, X) -->
	"(",
	regexp_phrase(0, X),
	")".

% Precedence 5 (lowest): Single Characters
regexp_phrase(5, literal(Char)) --> string_without("^$.|{}[]()*+?\\", [Char]).
regexp_phrase(5, wild)   --> ".".

% All rules at a given precedence are also rules at lower precedence.
regexp_phrase(Precedence, X) -->
	{
		Precedence =< 5,
		Next is Precedence + 1
	},
	regexp_phrase(Next, X).

:- use_module(library(dcg/basics)).
:- use_module('../src/rewrite').
:- use_module('../src/util').

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
	compile(Tree, NFA).


%! regexp(+Expression, +NFA)::match(+Input)
% Evaluate the regular expression against an input string. We use Thompson's
% algorithm to evaluate the NFA as a DFA (basically, we infer the powerset
% construction of the coresponding DFA from the NFA). During the rewrite
% process, the term is expanded to the form
%     regexp(Exp, NFA)::match(Input, CurrentState)
% where CurrentState is the state of the implicit DFA (represented as the
% list of all states that the NFA could be in given the previous input).
% In the worst case, there may be up to 2^S DFA states where S is the number
% of NFA states; thus the time to calculate the DFA state is O(2^S). However,
% since we're running a DFA, the number transitions considered is less-than
% or equal-to N where N is the length of the string. Thus the number of
% rewrites is at most N and the overall time complexity is O(N*2^S).
%
% Unlike some regular expression libraries (like the ones shiped with older
% versions of Python and Perl), there are fewer pathological cases for this
% algorithm. Some cases which can take over 60 seconds in Perl 5.8 take
% less-than 1 second using this construction.
%
% @see http://swtch.com/~rsc/regexp/regexp1.html

% Expand to the form `regexp(Exp, NFA)::match(Input, CurrentState)`
regexp(Exp, NFA)::match(Input) := regexp(Exp, NFA)::match(Input, StartState) :-
	setof(Q, null_path(NFA, start, Q), StartState).

% Terminate whenever we're in an accept state
regexp(_, _)::match(_, State) := true :- member(accept, State), !.

% The next state is the list of all reachable states of the NFA
% given the next symbol in the input, H
regexp(Exp, NFA)::match([H|T], State) := regexp(Exp, NFA)::match(T, Next) :-
	setof(Q, (
		member(Char, [H,wild]),
		member(Q0, State),
		member(edge(Q0,Q1,Char), NFA),
		null_path(NFA, Q1, Q)
	), Next),
	!.

% If no rewrites can be performed, fail
regexp(_,_)::match(_,_) := fail.


%! null_path(+NFA, +Source, ?To) is nondet
% To is a state in the NFA reachable from the Source state by following only
% epsilon/null transitions, i.e. a transition which consumes no input.

null_path(_, Source, Source).

null_path(NFA, Source, To) :- null_path(NFA, Source, To, [Source]).

null_path(NFA, Source, To, Seen) :-
	member(edge(Source,To,null), NFA),
	\+ member(To, Seen).

null_path(NFA, Source, To, Seen) :-
	member(edge(Source,Intermediate,null), NFA),
	\+ member(Intermediate, Seen),
	null_path(NFA, Intermediate, To, [Intermediate|Seen]).


% Compiler
% -------------------------

compile(Tree, NFA) :- compile(Tree, NFA, start, accept).

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

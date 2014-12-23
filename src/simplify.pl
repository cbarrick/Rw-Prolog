:- module(simplify, [
	simplify/2,
	simplify_args/2
]).


%% simplify(+Term, -Canonical)
% Simplifies a term to its normal form.

% Variables, numbers, atoms, and strings are already normal.
simplify(Term, Term) :- var(Term), !.
simplify(Term, Term) :- number(Term), !.
simplify(Term, Term) :- atom(Term), !.
simplify(Term, Term) :- string(Term), !.

% Do a graph-search for a normal form of a compound term.
simplify(Term, Canonical) :-
	compound(Term), !,
	simplify_compound(Term, Canonical).


%% simplify_compound(+Term, -Canonical)
% Performs a graph-search of the rewrite rules to find a normal form of a
% compound term.

simplify_compound(Term, Canonical) :-
	simplify_compound(Term, Canonical, [Term]).

simplify_compound(Term, Canonical, Seen) :-

	% We need to simplify the arguments first.
	simplify_args(Term, NormalArgs),

	(
		% Rules should not narrow variables. To prevent that, we check the
		% knowledge base against a copy of the term. Since the copy of the term
		% cannot be no wider than the original (in terms of variable bindings),
		% whenever the copy can subsume the original, the copy has not been made
		% any more specific.
		copy_term(NormalArgs, Pattern),
		clause(Pattern := Next, Body),
		subsumes_term(Pattern, NormalArgs),
		NormalArgs = Pattern,

		% Check that the conditions of the rewrite are met. We explicitly find
		% which module defines the rule so that we can call the body relative
		% to the proper module.
		predicate_property(Pattern := Next, imported_from(Module)),
		call(Module:Body),

		% Check that we have not already seen this form.
		\+ (
			member(Previous, Seen),
			Next == Previous
		),

		% Try to rewrite again.
		simplify_compound(Next, Canonical, [Next|Seen])
	;
		Canonical = NormalArgs
	),
	!.


%% simplify_args(+Term, +Canonical)
% Simplify the arguments of a term to their normal forms.

simplify_args(Term, Canonical) :-
	Term =.. [Functor|Arguments],
	length(Arguments, L),
	length(CanonicalArgs, L),
	maplist(simplify, Arguments, CanonicalArgs),
	Canonical =.. [Functor|CanonicalArgs],
	!.

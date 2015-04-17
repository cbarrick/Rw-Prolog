#!/usr/bin/env swipl -g init -t main

init :-
	write("Rw-Prolog, v0.0.1\n\n"),

	% load various core modules
	% TODO: investigate the autoloading mechanic in SWI
	use_module(library(apply)),  % maplist/2, etc
	use_module(library(gensym)), % gensym/2, etc
	use_module(library(lists)),  % member/2, length/2, etc
	use_module(library(random)), % random/1, maybe/1, etc

	% don't complain about discontiguous predicates
	% rewrite rules are often discontiguous
	style_check(-discontiguous),

	% Use traditional strings
	set_prolog_flag(double_quotes, codes),

	% load the rewrite library and scratchpad
	reload.


reload :-
	consult('src/rewrite.pl'),
	consult('scratchpad.pl').


main :-
	\+ main_loop.


main_loop :-
	flag(repl_count, Count ,Count+1),

	% Prompt for query
	prompt1("> "),
	read_term(user_input, Query, [
		syntax_errors(fail),
		variable_names(Names),
		singletons(_)
	]),

	% Halt on EOF
	(Query == end_of_file -> nl, !, fail ; true),

	% Add query to command-line history
	with_output_to(atom(Line), (
		write_term(Query, [quoted(true),variable_names(Names)]),
		write(".")
	)),
	rl_add_history(Line),

	% Evaluate
	call_rw(Query, Normal),
	print_term(Normal, [write_options([variable_names(Names)])]),
	write(" "),

	% Backtrack on any user input other than ".", "a", or enter.
	deterministic(Det),
	(
		Det == false,
		get_single_char(C),
		\+ member(C, [13,46,97])
	->
		ansi_format([bold], ";\n", []),
		fail
	;
		ansi_format([bold], ".\n\n", []),
		!, main_loop
	).
main_loop :-
	ansi_format([bold,fg(red)], "false.\n\n", []),
	main_loop.

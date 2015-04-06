#!/usr/bin/env swipl -g reload -t main


reload :-
	consult('src/rewrite.pl'),
	consult('scratchpad.pl'),
	style_check(-singleton).


main :-
	write("Rw-Prolog, v0.0.1\n"),
	main_loop.
main.


main_loop :-
	repeat,
	flag(repl_count, Count ,Count+1),

	% Prompt for query
	nl,
	prompt1("> "),
	read_clause(user_input, Query, [
		syntax_errors(fail),
		variable_names(Names)
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
	(
		call_rw(Query, Normal)
	*->true;
		ansi_format([bold,fg(red)], "false.\n", []),
		fail
	),
	print_term(Normal, [write_options([variable_names(Names)])]),
	write(" "),

	% Backtrack on any user input other than ".", "a", "A", or enter.
	deterministic(Det),
	(
		Det == false,
		get_single_char(C),
		\+ member(C, [13,46,65,97])
	->
		ansi_format([bold], ";\n", []),
		fail
	;
		ansi_format([bold], ".\n", [])
	),
	fail.

:- module(rw_modules, [
	rw_consult/1
]).

:- use_module('./rewrite.pl').


%! rw_consult(+Filename) is det
%
% TODO: Document

rw_consult(Filename) :-
	open(Filename, read, File),
	(true; close(File), fail), % cleanup handler if consulting fails
	read_term(File, (:- module(Module)), []),
	add_import_module(Module, rewrite, start),
	\+ rw_consult_mainloop(File, Module),
	close(File),
	!.

rw_consult_mainloop(File, Module) :-
	repeat,
	read_term(File, Term, []),
	(Term == end_of_file ->
		!, fail
	; Term = (:- Directive) ->
		Module:call_rw(Directive)
	;
		Module:assert(Term)
	),
	fail.

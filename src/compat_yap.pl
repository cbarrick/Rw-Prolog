% SWI-Prolog compatibility layer for YAP Prolog
%
% This code is a modification of the compatibility layer by David Reitter
% which, in turn, is based on code from SWI-Prolog by Jan Wielemaker.
%
% See http://www.david-reitter.com/compling/prolog/compat.html
%
%
% Authors:        David Reitter
% Copyright (C): 2003 Media Lab Europe
%
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU General Public License
% as published by the Free Software Foundation; either version 2
% of the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with this library; if not, write to the Free Software
% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
%
% As a special exception, if you link this library with other files,
% compiled with a Free Software compiler, to produce an executable, this
% library does not by itself cause the resulting executable to be covered
% by the GNU General Public License. This exception does not however
% invalidate any other reasons why the executable file might be covered by
% the GNU General Public License.


:- use_module(library(system)).
:- use_module(library(lists)).
:- use_module(library(charsio)).


% FLAG AND GENSYM

flag(Sym, Old, NewExp) :-
	((bb_get(Sym, Old), !) ; Old=0),
	New is NewExp,
	bb_put(Sym, New).

gensym(Base, Atom) :-
	atom_concat('gs_', Base, Key),
	flag(Key, N, N+1),
	number_atom(N, NA),
	atom_concat(Base, NA, Atom).


% STRINGS

string_to_atom(String, Atom) :-
	atom_codes(Atom, String).
string(S) :- is_codes(S).

is_codes(X) :-
		var(X), !,
		fail.
is_codes([]).
is_codes([N|T]) :-
	integer(N),
	N>0,
	N<256,
		is_codes(T).


% INTERNAL DATABASE

recordz(K,V) :-
	recordz(K,V,_).

recorded(K,V) :-
	recorded(K,V,_).


% EVALUATION

:- dynamic(register_arithmetic/1).

arithmetic_function(Functor/Arity) :-
	%print(register_arithmetic(Functor/Arity)),nl,
	retractall(register_arithmetic(Functor/Arity)),
	assertz(register_arithmetic(Functor/Arity)).

eval_arith(Term, Result) :-
	Term =.. [F|Args],
	eval_arith2(Args, Args2),
	((length(Args2,L),
	register_arithmetic(F/L),
	append(Args2, [Result], Args3),
	ATermN =.. [F|Args3],
	ATermN, !)
	;
	(ATerm =.. [F|Args2],
	Result is ATerm		% try internal eval routine
	)), !.

eval_arith2([], []).
eval_arith2([Term|Rest], [TermResult|RestResult]) :-
	eval_arith(Term, TermResult),
	eval_arith2(Rest, RestResult).

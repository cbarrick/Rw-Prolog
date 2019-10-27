:- consult('lib/fib.pl').
:- consult('lib/infinite.pl').
:- consult('lib/regexp.pl').
:- consult('lib/rw_lists.pl').

a := b.
b := c.
b := d.
c := c.
d.

x := w.
x := y.
y := z.
z.
w.

return(_).

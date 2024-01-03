Define a let** construct that behaves like the standard let*, but gives to variables provided without a binding
the value of the last defined variable.
It also contains a default value, stated by a special keyword def:,
to be used if the first variable is given without binding.

For example:

(let** def: #f (a (b 1) (c (+ b 1)) d (e (+ d 1)) f)
(list a b c d e f))

should return '(#f 1 2 2 3 3), because a assumes the default value #f, while d = c and f = e.

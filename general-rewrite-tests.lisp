(load "general-rewrite.lisp")
(load "example-combinators.lisp")

(setf *verbose-unit-test* t)
(get-combinators *rules*)

(unit-test (unfold-combinator '(M I) *rules*))
(unit-test (unfold-combinator '(I) *rules*))
(unit-test (unfold-combinator '(X) *rules*))

(unit-test (print-all (all-rewrites '(M (I I) (I X))
				    *rules*)))

(unit-test (all-rewrites 'M *rules*))
(unit-test (all-rewrites '(M X) *rules*))
(unit-test (print-all (all-rewrites '(I (M X) (K X Y)) *rules*)))

(unit-test (print-all (all-rewrites '(B M M (B M M)) *rules*)))
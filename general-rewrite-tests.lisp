(load "general-rewrite.lisp")
(load "example-combinators.lisp")

(setf *verbose-unit-test* t)
(get-combinators *rules*)

(unit-test (unfold-combinator '(M I) *rules*))
(unit-test (unfold-combinator '(I) *rules*))
(unit-test (unfold-combinator '(X) *rules*))
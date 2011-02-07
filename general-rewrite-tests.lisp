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

(unit-test (print-all (all-rewrites-many '(I (M I) (K X Y)) *rules* 5)))


(unit-test (print-all (all-traces '(I (M X)) *rules* 3)))
(unit-test (print-all (all-traces '(B M M (B M M)) *rules* 3)))

(setf bmm '(B M M (B M M)))
(setf a (all-traces bmm *rules* 3))
(print-all a)
(member (simplify-expression `(,bmm ,bmm)) a :test #'equal)
(setf b (all-traces `(,bmm ,bmm) *rules* 0))

;; YEAH !!!!!
(intersection a b :test #'equal :key #'simplify-expression)


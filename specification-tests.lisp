(load "specification.lisp")
(load "example-combinators.lisp")

(unit-test (such-that->equality '(V Y = X)))


(unit-test (build-comb L :such-that (L = L L)
		         :using (S K)
		         :search-type first
			 :count-from 3
			 :need-traces t
			 :count-to   5)) 

;; this one doesn't work if the rule-db is not properly initialized
(unit-test (express B :using (S K)
		    :search-type first
		    :count-from 1
		    :need-traces t
		    :count-to 10))

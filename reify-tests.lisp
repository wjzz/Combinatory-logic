(load "reify.lisp")

; (load "example-combinators.lisp")

(reset-def-db)
(rcomb S x y z = x z (y z))
(rcomb K x y = x)
(rcomb I x = x)

(let ((spec (express I :using (S K) :count-from 3 :count-to 4)))
  (reify spec))

(unit-test (reify (build-comb X :such-that (X a = a) :search-type first :using (S K))))


(reset-def-db) 
(rcomb B x y z = x (y z))
(rcomb C x y z = x z y)
(rcomb T x y   = y x)

(unit-test (reify 
	    (express C :using (B T)
		       :count-to 8
	    )))
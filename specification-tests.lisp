(load "specification.lisp")
(load "example-combinators.lisp")

(get-combinators (get-def-db))
(setf code `(express T :using (S K)))
(macroexpand code)
(eval code)

(unit-test (express B :using (S K)
		      :search-type first
		      :count-from 1
		      :count-to 10))

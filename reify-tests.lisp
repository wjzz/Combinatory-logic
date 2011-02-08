(load "reify.lisp")

; (load "example-combinators.lisp")

;; SBCL --script
;; CLISP -C 

(setf *verbose-unit-test* nil)


;; A very simple example

(reset-def-db)
(rcomb S x y z = x z (y z))
(rcomb K x y = x)
(rcomb I x = x)


(defun ski ()
  
  (let ((spec (express I :using (S K) :count-from 1 :count-to 4)))
    (unit-test (reify spec)))
  
  (unit-test (reify (build-comb X :such-that (X a = a) :search-type all :using (S K)))))

(unit-test (ski))


;; A non trivial example, Church have found it aroung 1941

(reset-def-db)
(rcomb B x y z = x (y z))
(rcomb C x y z = x z y)
(rcomb T x y   = y x)

(defun church ()

  (let ((church-1941
    (express C :using (B T)
	     :count-to 8
	     )))

  ;; 11s when compiled with CLISP
  (unit-test (reify church-1941 :verbose nil :max-depth 7))))

(setf *verbose-unit-test* t)
(church)


;; an interesting example, because the rewriting diverges, so a more powerful prover is needed

(reset-def-db)
(rcomb M x = x x)
(rcomb B x y z = x (y z))

(defun simple-diverging ()
  
  (let ((diverging (build-comb X
			       :such-that (X = X X)
			       :search-type all
			       :using (M B)
			       :need-traces t
			       :count-to 6
			       )))
    (unit-test (reify diverging :verbose nil :max-depth 3))))

(setf *verbose-unit-test* t)
(simple-diverging)


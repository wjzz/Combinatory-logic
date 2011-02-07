(load "reify.lisp")

; (load "example-combinators.lisp")

(setf *verbose-unit-test* t)

(reset-def-db)
(rcomb S x y z = x z (y z))
(rcomb K x y = x)
(rcomb I x = x)


(defun ski ()
  
  (let ((spec (express I :using (S K) :count-from 3 :count-to 4)))
    (unit-test (reify spec)))
  
  (unit-test (reify (build-comb X :such-that (X a = a) :search-type all :using (S K)))))


; (unit-test (ski))

(reset-def-db) 
(rcomb B x y z = x (y z))
(rcomb C x y z = x z y)
(rcomb T x y   = y x)

(defun church ()

  (defvar church-1941
    (express C :using (B T)
	     :count-to 8
	     ))

  ;; 11s when compiled  
  (unit-test (reify church-1941 :verbose t :max-depth 7)))

; (church)



(rcomb L x y = x (y y))

(defun onlyLLLL ()

  (defvar only-Ls 
    (build-comb X
	      :such-that (X X = X)
	      :search-type all
	      :using (L)
	      :count-to 12
	      ))

  (unit-test (reify only-Ls :verbose t :max-depth 2))
  )

; (unit-test (onlyLLLL))

; (unit-test (length (generate-combs 12 '(L))))


;; an interesting example, because the rewriting diverges

(rcomb M x = x x)
(rcomb B x y z = x (y z))

(defun simple-diverging ()
  
  (let ((diverging (build-comb X
			       :such-that (X = X X)
			       :search-type first
			       :using (M B)
			       :count-to 6
			       )))
    (unit-test (reify diverging :verbose :t :max-depth 7))))

; (simple-diverging)


;; this a nasty example, because the rewriting diverges
(defun prove-with-history (comb1 comb2)
  (let
      ((a (multiple-value-list (full-rewrite comb1 :max-depth 10)))
       (b (multiple-value-list (full-rewrite comb2 :max-depth 10))))
    (intersection (second a) (second b) :test #'equal)))

(defvar lft '(B M M (B M M)))
(defvar rht '((B M M (B M M))(B M M (B M M))))
(unit-test (prove-with-history lft rht))


(rcomb I x = x)
(unit-test (prove-with-history '(M I) '(M (M (M I)))))

(unit-test (full-rewrite '(M (M (M I)))))

(load "rewrite.lisp")

; Some combinators 
(reset-def-db)

(rcomb M x = x x)
(rcomb I x = x)
(rcomb K x y = x)
(rcomb B x y z = x (y z))
(rcomb T x y z = x (z y))
(rcomb S x y z = x z (y z))
(print-def-db)
(get-combinators *rules*)


;; SIMPLIFY EXPRESSION
(unit-test (simplify-expression '((((A))))))
(unit-test (simplify-expression '(((A B) C) D)))
(unit-test (simplify-expression '((comp A B) C)))


;; REWRITES

(print-def-db)
(rewrite '(I M))
(rewrite '(M (M M)))


;; (full-rewrite '(M M))
;; (full-rewrite '(M I I I I I))
;; (full-rewrite '(M M (I I I))) ;; as opposed to
;; (lazy-rewrite '(M M (I I I))) ;; which is not as reduced
;; (full-rewrite '(M (M (M (M (M (M I)))))) :max-depth 6)
;; (full-rewrite '(M I) :max-depth 2 :print-trace t)


;; ;; TESTS
;; (print "-----------------")
;; (lazy-rewrite '((comp M M) I) :print-trace 1)

;; ;; this a nasty example, because the rewriting diverges
;; (lazy-rewrite '((comp M M) (comp M M)) :print-trace 1 :max-depth 20)

;; (lazy-rewrite '(M (M (M (M (M M)))))   :print-trace 1)
;; (lazy-rewrite '((comp (comp M M) M) I) :print-trace 1 :max-depth 20)

;; ;; a test that shows that rewrite doesn't eval everything possible
;; (lazy-rewrite '((M M) (I I))) ;; should be (M M) I
;; (full-rewrite '((M M) (I I))) ;; should be (M M) I
;; (lazy-rewrite '(I I (M M)))

;; ;; tests of simplify
;; (list (equal 'X 
;; 	    (simplify-expression '((((X))))))
;;      (equal '(A B C D) 
;; 	    (simplify-expression '(((A B) C) D)))
;;      (equal '(A B (C D E))
;; 	    (simplify-expression '((A B) ((C D) E)))))

;; (simplify-expression '((A B) C))

;; (full-rewrite '(I x y))

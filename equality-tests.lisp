(load "equality.lisp")
(load "example-combinators.lisp")

(setf *verbose-unit-test* t)

(unit-test (equality (x y) (y x)))


(save-equality multi-m
	      I
	      (M I))


(unit-test (prove-equality-by-rewriting multi-m))
 ;; OK!
(unit-test (prove-equality multi-m))


(unit-test (prove-equality-by-rewriting (equality M I)) )
;; OK! (can't be done)

(save-equality many-ms I (M (M (M (M (M I))))))
(unit-test (prove-equality-by-rewriting many-ms 5))
;; success!


(unit-test (prove-equality (equality (M M I) (M M (I I I)))))
;; success, because it uses reduces as much as possible

(unit-test (prove-equality (equality (x y z) ((x y) z))))

(unit-test (prove-equality (equality (B B B x y z w) (x (y z w)))))


;; a troublesome test since it can't be proven by lazy rewriting of just anything
(let 
    ((bmm '(B M M (B M M))))
  (unit-test (prove-equality-traces (make-equality-struct :left bmm :right `(,bmm ,bmm))
				    *rules*
				    )))
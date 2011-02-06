(load "rewrite.lisp")
(load "example-combinators.lisp")

;; REWRITES

(unit-test (get-combinators *rules*))
(unit-test (rewrite '(I M)))
(unit-test (rewrite '(M (M M))))
(unit-test (rewrite '(X (I M))))
(unit-test (rewrite '((comp I M) X))) ;; this mustn't work
(unit-test (rewrite '(B I M X))) ;; this is the correct way


(unit-test (full-rewrite '(M M)))
(unit-test (full-rewrite '(M I I I I I)))

;; compare full and lazy - lazy doesn't evaluate it's arguments
(unit-test (full-rewrite '(M M (I I I))))
(unit-test (lazy-rewrite '(M M (I I I))))

(unit-test (full-rewrite '(M (M (M (M (M (M I)))))) :max-depth 6))
(unit-test (full-rewrite '(M I) :max-depth 2 :print-trace t))


(unit-test (lazy-rewrite '(B M M I) :print-trace 1))




(unit-test (lazy-rewrite '(M (M (M (M (M M)))))))
(unit-test (full-rewrite '(M (M (M (M (M M)))))))

(unit-test (lazy-rewrite '(B (B M M) M I) :print-trace 1 :max-depth 16))

;; a test that shows that rewrite doesn't eval everything possible
(unit-test (lazy-rewrite '((M M) (I I)))) ;; should be (M M) I
(unit-test (full-rewrite '((M M) (I I)))) ;; should be (M M) I

;; an important difference
(unit-test (lazy-rewrite '(X (I I (M M)))))
(unit-test (full-rewrite '(X (I I (M M)))))
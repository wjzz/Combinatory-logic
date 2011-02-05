(load "generate.lisp")
(load "specification.lisp")

(defun test-steps (count available-combs)
  (time (generate-combs count available-combs))
  (time (mapcar #'tree->nested-lists (generate-combs count available-combs)))
  (time (mapcar (lambda (x) (full-rewrite (tree->nested-lists x)))
		(generate-combs count available-combs))))

; (test-steps 12 '(I))

;(get-combinators *rules*)
;(rcomb L x y = x (y y))

; (mapcar (lambda (x) (lazy-rewrite (tree->nested-lists x))) (generate-combs 5 '(L)))
;(defparameter DD-thm (build-theorem 'D (spec (D) (D) (L))))

(defun prove-DD (count)
  (rcomb L x y = x (y y))
  (let 
      ((combs (generate-combs count '(L))))
    (remove-if-not
     (lambda (c)
       (let* ((comb (tree->nested-lists c))
    	      (thm (build-theorem comb (make-specification :parameters (list comb) :body (list comb) :combinators '(L)))))
;	 (write thm)
;	 (format t "~%~%")
    	 (prove-theorem-by-rewriting thm 20 #'full-rewrite)))
     combs)))


(defun prove-C (count)
  (rcomb T x y = y x)
  (rcomb B x y z = x (y z))
  (let 
      ((combs (generate-combs count '(B T))))
    (remove-if-not
     (lambda (c)
       (let* ((comb (tree->nested-lists c))
    	      (thm (build-theorem comb (spec (x y z) (x z y) (B T)))))
;	 (write thm)
;	 (format t "~%~%")
    	 (prove-theorem-by-rewriting thm 50))) ; #'lazy-rewrite)))
     combs)))


(dotimes (n 13)
  (format t "~%Trying n = ~d~%" n)
  (let ((result (prove-DD n)))
    (if result
	(write (mapcar #'tree->nested-lists result)))))


(defun reify (spec &optional (max-count 5))
  (defun iter (count)
    (if (> count max-count)
	nil
	(let* ((candidates (generate-combs count (specification-combinators spec)))
	       (results (remove-if-not (lambda (comb)
					 (prove-theorem (build-theorem comb spec)))
				       candidates)))
	  (append (remove-duplicates results
				     :test (lambda (x y) (equal (simplify-expression x)
								(simplify-expression y))))
		  (iter (1+ count))))))
  (iter 1))





(setf B (spec (x y z) (x (y z)) (B)))
; (reify B)


(setf D (spec (x y z w)
	     (x y (z w))
	     (B)))

; (reify D)


(setf triple (spec (x y z w)
		   (x (y z w))
		   (B)))
; (reify triple)

(setf I (spec (x) (x) (S K)))

(prove-theorem (theorem (S K K x) x))
; (reify I)

(rewrite '(S K S x))
(rewrite (rewrite '(S K K x)))

; (reify (spec (x y z) (x (y z)) (S K I)) 4)

(load "specification.lisp")
(load "generate.lisp")

(defun try-candidate (spec comb)
  (flet
      ((subst-cand (expr)
	 (simplify-expression (subst comb (specification-comb-name spec) expr))))
    (let ((thm (map-equality (specification-equality spec)
			     #'subst-cand)))
      ; (format t "~a~%" thm)
      (prove-equality thm))))



(defun reify (spec)
  (let ((max-count (specification-count-to spec))
	(find-all? (eql 'all (specification-search-type spec))))
    
    (defun reify-iter (count)
      (if (> count max-count)
	  nil
	  (let* ((candidates (generate-combs count (specification-using spec)))
		 (passed (remove-if-not (lambda (comb) (try-candidate spec comb))
					candidates)))
	    (cond (find-all?
		   (append passed (reify-iter (1+ count))))

		  ;; return the first one only
		  (passed 
		   (first passed))

		  ;; try harder
		  (t (reify-iter (1+ count)))))))
		  
    (reify-iter (specification-count-from spec))))


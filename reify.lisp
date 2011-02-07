(load "specification.lisp")
(load "generate.lisp")

(defun try-candidate (spec comb pred &optional (max-depth 5))
  (flet
      ((subst-cand (expr)
	 (simplify-expression (subst comb (specification-comb-name spec) expr))))
    (let ((thm (map-equality (specification-equality spec)
			     #'subst-cand)))
      ; (format t "~a~%" thm)
      ;(prove-equality thm 'strict max-depth))))
      (funcall pred thm max-depth))))



(defun reify (spec &key verbose (max-depth 5))
  (let ((max-count (specification-count-to spec))
	(pred (if (specification-need-traces spec)
		  (lambda (thm max-depth)
		    (prove-equality-traces thm *rules* max-depth))
		  (lambda (thm max-depth) 
		    (prove-equality thm 'strict max-depth))))		
	(find-all? (eql 'all (specification-search-type spec))))
    
    (defun reify-iter (count)
      (if (> count max-count)
	nil
	(progn
	  (when verbose
	    (format t "Checking with count = ~d and max-depth = ~d ~%" count max-depth))
	  (let* ((candidates (generate-combs count (specification-using spec)))
		 (passed (remove-if-not (lambda (comb) (try-candidate spec 
								      comb
								      pred
								      max-depth))
					candidates)))
	    (cond (find-all?
		   (append passed (reify-iter (1+ count))))

		  ;; return the first one only
		  (passed 
		   (first passed))

		  ;; try harder
		  (t (reify-iter (1+ count))))))))
		  
    (reify-iter (specification-count-from spec))))

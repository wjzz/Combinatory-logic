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
  (defun reify-iter (count max-count)
    (if (> count max-count)
	nil
	(let* ((candidates (generate-combs count (specification-using spec)))
	       (passed (remove-if-not (lambda (comb) (try-candidate spec comb))
				      candidates)))
	  (if passed
	      passed
	      (reify-iter (1+ count) max-count)))))
  (reify-iter (specification-count-from spec)
	      (specification-count-to   spec)))

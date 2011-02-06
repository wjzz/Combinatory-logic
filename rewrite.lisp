(load "rule_db.lisp")

;; ------------------------------
;; --  SIMPLIFING EXPRESSIONS  --
;; ------------------------------


(defun simplify-expression (expression)
  "Simplifies a given expression of combinatory logic.

 Example transformations:
 ((X)) -> X
 ((A B) C) -> (A B C)"
  (cond 
    ((atom expression)
	 expression)

    ((consp expression)
     (cond
       ; A singleton list doesn't need parens
       ; (X) -> X
       ((null (rest expression))
	(simplify-expression (first expression)))

       ; a nested application can be flattened
       ; ((A B) C) -> (A B C)
       ((consp (first expression))
	(simplify-expression (append (first expression)
				     (rest expression))))

       ; apply simplify to subexpressions and check if simplify changed anything
       ; (A ((B C) D) -> (A (B C D))
       (t 
	(let ((result-expression (mapcar #'simplify-expression expression)))
	  (if (equal expression result-expression)
	      expression
	      (simplify-expression result-expression))))))))



;; -----------------
;; --  REWRITING  --
;; -----------------


(defun rewrite-step (expression rule-db)
  "Makes a single rewrite based on the rule-db. Uses the normal evaluation strategy, so the outermost combinator is expanded."
  (cond 
    ((atom expression) expression)
    ((consp expression)
     (cond
       ((consp (first expression))
	      (cons (rewrite-step (first expression) rule-db)
		    (rest expression)))

       ;; If we found a combinator we want to rewrite the expression using the definition of the combinator
       ((atom (first expression))
	(let 
	    ((combinator (find-combinator (first expression) rule-db)))

	  ;; If the atom is not in the combinator base we assume it's a parameter variable and we don't touch it
	  (if (null combinator)
	      expression
	      (let*
		  ((parameters (combinator-parameters combinator))
		   (split-parameters (break-at (length parameters) (rest expression)))
		   (values       (car split-parameters)))

		;; We have to check if enough parameters were provided
		;; If not, we don't apply the combinator
		(if (< (length values)
		       (length parameters))
		    expression
		    
		    ;; If K is a binary combinator and K x y ==> e then
		    ;; (K x y A B ... Z) ==> (e A B ... Z)
		    (let* 
			((rest-of-expr (cdr split-parameters))
			 (body         (combinator-body combinator))
			 (env          (create-environment parameters values)))
		      (simplify-expression (cons (substitute-values body parameters env)
						 rest-of-expr))))))))))))


(defun rewrite (expression)
  (rewrite-step expression *rules*))




(defun lazy-rewrite (expression &key print-trace (max-depth 10))
  (when print-trace
     (print expression))
  (if (= 0 max-depth)
      expression
      (let ((result (rewrite-step expression *rules*)))
	(if (equal expression result)
	    result
	    (lazy-rewrite result :print-trace print-trace :max-depth (1- max-depth))))))




(defun full-rewrite (expression &key print-trace (max-depth 10))
;  (when print-trace
;    (print expression))
  (if (= 0 max-depth)
      expression
      (cond ((atom expression) expression)
	    ((consp expression)
	     (let*
		 ((exp2 (mapcar (lambda (e)
				  (full-rewrite e :print-trace print-trace :max-depth (1- max-depth)))
				expression))
		  (result (rewrite-step exp2 *rules*)))
	       (when print-trace 
		 (print exp2))
	       (if (equal exp2 result)
		   result
		   (full-rewrite result :print-trace print-trace :max-depth (1- max-depth))))))))

(load "rule_db.lisp")

; Some combinators 
(reset-def-db)

(rcomb M x = x x)
(rcomb I x = x)
(rcomb K x y = x)
(rcomb B x y z = x (y z))
(rcomb T x y z = x (z y))
(rcomb S x y z = x z (y z))
;(print-def-db)
;(get-combinators *rules*)


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
       ; notice that if A = comp then this tranformation cannot be performed 
       ; (in the general case)!
       ((and (consp (first expression))
	     (not (eql 'comp (first (first expression)))))
	(simplify-expression (append (first expression)
				     (rest expression))))

       ; apply simplify to subexpressions and check if simplify changed anything
       (t 
	(let ((result-expression (mapcar #'simplify-expression expression)))
	  (if (equal expression result-expression)
	      expression
	      (simplify-expression result-expression))))))))


(simplify-expression '((((A)))))
(simplify-expression '(((A B) C) D))
(simplify-expression '((comp A B) C))


;; -----------------
;; --  REWRITING  --
;; -----------------


(defun compositionp (expression)
  (and (consp expression)
       (eq 'comp (first expression))))


;; TODO this looks hard to read
;; It might be agood idea to introduce more predicates like compositionp

(defun rewrite-step (expression rule-db)
  "Makes a single rewrite based on the rule-db. The highest term is rewritten."
  (cond 
    ((atom expression) expression)
    ((consp expression)
     (cond
       ((compositionp (first expression))
	(let* ((comp-expr (first expression))
	       ; comp-expr = ('comp outer inner)
	       (outer     (second comp-expr))
	       ; TODO this will work only for exactly two functions composed
	       (inner     (third  comp-expr)))
	  (list outer (list inner (rest expression)))))       
	  
       ((consp (first expression))
	      (cons (rewrite-step (first expression) rule-db)
		    (rest expression)))

       ((atom (first expression))
	(let 
	    ((combinator (find-combinator (first expression) rule-db)))
	  ;; If the atom is not the base we assume it's parameter variable and leave it
	  (if (null combinator)
	      expression
	      (let*
		  ((parameters (combinator-parameters combinator))
		   (split-parameters (break-at (length parameters) (rest expression)))
		   (values       (car split-parameters)))
		(if (< (length values)
		       (length parameters))
		    expression
		    (let* 
			((rest-of-expr (cdr split-parameters))
			 (body         (combinator-body combinator))
			 (env          (create-environment parameters values)))
		      (simplify-expression (cons (substitute-values body parameters env)
						 rest-of-expr))))))))))))


(rewrite-step (rewrite-step '(M I) *rules*) *rules*)

(defun rewrite (expression)
  (rewrite-step expression *rules*))

(print-def-db)
(rewrite '(I M))
(rewrite '(M (M M)))

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



(full-rewrite '(M M))
(full-rewrite '(M I I I I I))
(full-rewrite '(M M (I I I))) ;; as opposed to
(lazy-rewrite '(M M (I I I))) ;; which is not as reduced
(full-rewrite '(M (M (M (M (M (M I)))))) :max-depth 6)
(full-rewrite '(M I) :max-depth 2 :print-trace t)


;; TESTS
(print "-----------------")
(lazy-rewrite '((comp M M) I) :print-trace 1)

;; this a nasty example, because the rewriting diverges
(lazy-rewrite '((comp M M) (comp M M)) :print-trace 1 :max-depth 20)

(lazy-rewrite '(M (M (M (M (M M)))))   :print-trace 1)
(lazy-rewrite '((comp (comp M M) M) I) :print-trace 1 :max-depth 20)

;; a test that shows that rewrite doesn't eval everything possible
(lazy-rewrite '((M M) (I I))) ;; should be (M M) I
(full-rewrite '((M M) (I I))) ;; should be (M M) I
(lazy-rewrite '(I I (M M)))

;; tests of simplify
(list (equal 'X 
	    (simplify-expression '((((X))))))
     (equal '(A B C D) 
	    (simplify-expression '(((A B) C) D)))
     (equal '(A B (C D E))
	    (simplify-expression '((A B) ((C D) E)))))

(simplify-expression '((A B) C))

(full-rewrite '(I x y))

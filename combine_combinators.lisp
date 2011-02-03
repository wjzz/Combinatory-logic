(load "theorems.lisp")

;; TODO  define macros

(defstruct specification
  :parameters
  :body
  :combinators)

(defmacro spec (from obtain using)
  `(make-specification
    :parameters  ',from
    :body        ',obtain
    :combinators ',using))

(defvar test-spec '(spec (x y z) (x (y z)) (B)))
(macroexpand-1 test-spec)
(eval test-spec)




;; use case
; (defvar D (spec D x y z = x z y))

;; now we want to be able to generate such D by combining other combinators
;(reify D 
;       using B M I 
;       in 100 steps) ;; generate a D by applying only B, M and I
;; returns nil if we werent able to find one

(defun mappend (f lst)
  ;(apply #'append (mapcar f lst)))
  (reduce #'append (mapcar f lst) :initial-value nil))

(mappend (lambda (x) (range-from-zero x)) '(1 2 3 4))

(defun partitions-iter (n k)
  "Returns a list of all partitions of the number n into k non-empty and less than n slots.
  A parition is represented by a list of values in the respective slots."
  (cond ((= 0 k) nil)
	((= 0 n) nil)
	((= 1 k) `((,n)))
	((< 1 k)
	 (let 
	     ((firsts (range 1 (1- n))))
	   (mappend (lambda (first) 
		      (mapcar (lambda (solution)
				(cons first solution))
			      (partitions-iter (- n first) (1- k))))
		    firsts)))))

(partitions-iter 10 0)
(partitions-iter 10 1)
(partitions-iter 4 3)

(defun partitions (n)
  "Returns a list of all partitions of n into at least 2 non-empty stots."
  (mappend (lambda (k) (partitions-iter n k))
	   (range 2 n)))
		
	   
(partitions 4)

(defun cart-prod (lst)
  (if (null lst) 
      (list nil)
      (let ((smaller (cart-prod (rest lst)))
	    (heads   (first lst)))
	(mappend (lambda (x) 
		  (mapcar (lambda (solution)
			    (cons x solution))
			  smaller))
		heads))))

(cart-prod '((1 2) (a b c)))



;; basic case
(defun generate-combs (count available-combs)
  (cond ((= 1 count) available-combs)
	((< 1 count) 
	 (let ((parts (partitions count)))
	   (mappend (lambda (partition)
		      (cart-prod (mapcar (lambda (cnt)
					   (generate-combs cnt available-combs))
					 partition)))
		    parts)))))
	   
      

(generate-combs 2 '(M B I))




(defun build-theorem (comb spec)
  (make-theorem-struct :left  (specification-body spec)
		       :right (cons comb (specification-parameters spec))))

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
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



(defun combine-results (xs ys acc)
  (let ((result acc))
    (dolist (x xs)
      (dolist (y ys)
	(push (cons x y) result)))
    result))

;T (combine-results '(1 2) '(a b c) '(start))


(defun generate-less-mem (count available-combs table)
  (let ((result (gethash count table)))
    (if result
	result
	(let ((new-result (generate-less-mem-iter count available-combs table)))
	  (setf (gethash count table) new-result)
	  new-result))))

(defun generate-less-mem-iter (count available-combs table)
  (if (= 1 count)
      available-combs
      (let (results)
	(dotimes (n (1- count))
	  (let ((left  (generate-less-mem (1+ n)        available-combs table))
		(right (generate-less-mem (- count n 1) available-combs table)))
	     (setf results (combine-results left right results))))
	results)))

(defun generate-combs (count available-combs)
  (let ((table (make-hash-table)))
    (generate-less-mem-iter count available-combs table)))

(generate-combs 2 '(M B I))






;; use case
; (defvar D (spec D x y z = x z y))

;; now we want to be able to generate such D by combining other combinators
;(reify D 
;       using B M I 
;       in 100 steps) ;; generate a D by applying only B, M and I
;; returns nil if we werent able to find one


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
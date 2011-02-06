(load "rewrite.lisp")

(defstruct theorem-struct
  :left
  :right)

(defmacro theorem (left right)
  `(make-theorem-struct :left ',left
			:right ',right))

(theorem (x y) (y x))

(defmacro save-theorem (name left right)
  `(setf ,name (theorem ,left ,right)))

(macroexpand-1 `(save-theorem simple I (M M)))
(save-theorem simple I (M M))
simple

(save-theorem multi-m
	      I
	      (M I))

(defun prove-theorem-by-rewriting (thm &optional (max-depth 10) (rewriting-func #'full-rewrite))
  (let
      ((left  (funcall rewriting-func (simplify-expression (theorem-struct-left  thm)) :max-depth max-depth))
       (right (funcall rewriting-func (simplify-expression (theorem-struct-right thm)) :max-depth max-depth)))
    (if (equal (simplify-expression left) 
	       (simplify-expression right))
	'success
	nil)))

(defun prove-theorem (thm &optional (norm-strategy 'strict) (max-depth 10))
  (let 
      ((rewrite-func (case norm-strategy
		       ((strict)   #'full-rewrite)
		       (otherwise  #'lazy-rewrite))))
    (prove-theorem-by-rewriting thm max-depth rewrite-func)))


(prove-theorem-by-rewriting multi-m) ;; OK!
(prove-theorem-by-rewriting (theorem M I)) ;; OK! (can't be done)

(save-theorem many-ms I (M (M (M (M (M I))))))
(prove-theorem-by-rewriting many-ms 5)  ;; success!

(prove-theorem multi-m)
(prove-theorem (theorem (M M I) (M M (I I I)))) ;; success, because it uses reduces as much as possible
(prove-theorem (theorem (x y z) ((x y) z)))

(prove-theorem (theorem (B B B x y z w) (x (y z w))))

(lazy-rewrite '(B B B x y z w))
(full-rewrite '(B B B x y z w) :print-trace 1)
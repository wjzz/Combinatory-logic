(load "rewrite.lisp")

(defstruct equality-struct
  :left
  :right)

(defmacro equality (left right)
  `(make-equality-struct :left  ',left
			 :right ',right))


;; This macro should only be used for testing
(defmacro save-equality (name left right)
  `(setf ,name (equality ,left ,right)))


(defun map-equality (thm func)
  "Applies the given func to both sides of the equality."
  (let ((left (funcall func (equality-struct-left thm)))
	(right (funcall func (equality-struct-right thm))))
  (make-equality-struct :left left
			:right right)))

(defun equality-truep (thm)
  "Checks if boths sides of the equality are already equal (doesn't perfom any calculations)."
  (equal (equality-struct-left thm)
	 (equality-struct-right thm)))


(defun prove-equality-by-rewriting (thm &optional (max-depth 10) (rewriting-func #'full-rewrite))
  (flet 
      ((apply-rewrite (e) 
	 (simplify-expression (funcall rewriting-func (simplify-expression e) 
				                       :max-depth max-depth))))
    (if (equality-truep (map-equality thm #'apply-rewrite))
	'success
	nil)))



(defun prove-equality (thm &optional (norm-strategy 'strict) (max-depth 10))
  (let 
      ((rewrite-func (case norm-strategy
		       ((strict)   #'full-rewrite)
		       (otherwise  #'lazy-rewrite))))
    (prove-equality-by-rewriting thm max-depth rewrite-func)))


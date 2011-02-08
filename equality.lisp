;(load "rewrite.lisp")
(load "general-rewrite.lisp")

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



(defun prove-equality (thm &optional (norm-strategy 'strict) (max-depth 5))
  (let 
      ((rewrite-func (case norm-strategy
		       ((strict)   #'full-rewrite)
		       (otherwise  #'lazy-rewrite))))
    (prove-equality-by-rewriting thm max-depth rewrite-func)))



;; PROVING PROCEDURES FOR DIVERGING EXPRESSIONS

(defun traces-prove (thm rule-db depth)
  (if (zerop depth)
      (equality-truep thm)
      (let
	  ((left  
	    ;nil)
	    (all-tracesn (equality-struct-left thm) rule-db depth))
	   (right 
	    nil)
	   )
	(member (equality-struct-right thm)
		left
		:test #'equal)
	)))

(defun prove-equality-traces (thm rule-db &optional (max-depth 3) (depth 0))
  (traces-prove thm rule-db max-depth))


	    ;(all-tracesn (equality-struct-right thm) rule-db depth))
	  ;; ((modified-thm (map-equality thm 
	  ;; 			       (lambda (e)
	  ;; 				 ;; changed all-traces to all-tracesn
	  ;; 				 (all-tracesn e rule-db depth)))))

;; 	(intersection ;(equality-struct-left  modified-thm)
;; 		      ;(equality-struct-right modified-thm)
;; 		      ; this is a dangerous trick, it may make some theorems unprovable
;; 		      ; (list (equality-struct-right thm))
;; ;		      (list (equality-struct-left thm))
;; ;		      (list (equality-struct-right thm))
;; 	              left
;; 		      (list (equality-struct-right thm))
;; 		      ;right
;; 		      :test #'equal
		      
;; 		      ; :key #'simplify-expression
;		      )))


;; (defun prove-equality-traces (thm rule-db &optional (max-depth 3) (depth 0))
;;   (if (> depth max-depth)
;;       nil
;;       (if (traces-prove thm rule-db depth)
;; 	  'success
;; 	  (prove-equality-traces thm rule-db max-depth (1+ depth)))))



;; we will represent a simple application of B to A as (A B) [ that the trafitional notation ]
;; we will utilize macros to allow for this syntax

;; we want to state the rules for particular combinators
;; i.e. (M x) == (x x)

;; we need functions for evaluating/reducing expressions
;; however, notice that some terms may not be reductible to a "normal form" when all rules are applied
;; a single step function should come in handy for those monsters

;; syntax for defining combinators

(defstruct combinator
  :name
  :parameters
  :body)

(setf c (make-combinator 
	 :name 'M
	 :parameters '()))

(combinator-p c)

;; m'kay

(defun iter (lst before after seen-eq)
  (cond ((null lst) (cons (reverse before) (reverse after)))
	(seen-eq     (iter (cdr lst) before (cons (car lst) after) seen-eq))
	((eq (car lst) '=) (iter (cdr lst) before after t))
	(t  (iter (cdr lst) (cons (car lst) before) after nil))))

(defmacro comb (name &rest rest)
  ;; break the rest at '='
  (let*
      ((result (iter rest '() '() nil))
       (parameters (car result))
       (body       (cdr result)))
    `(make-combinator :name       ',name
		      :parameters ',parameters
		      :body       ',body)))

(macroexpand `(comb M x = x x))
(comb M x = x x)
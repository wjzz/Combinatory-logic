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
(setf M (comb M x = x x))
(setf I (comb I x = x))
M

(M @ I)


(defmacro install-comb (name &rest rest)
  ;; break the rest at '='
  (let*
      ((result (iter rest '() '() nil))
       (parameters (car result))
       (body       (cdr result)))
    `(defun ,name ,parameters ,body)))
  ; `(setf (symbol-function ,name)
;	   #'(lambda ,parameters ,body))))

(macroexpand-1 `(install-comb A x y = x y))

(install-comb
 M x = x x)

(M M)


(subst '(1 2) 'a '(a b a))

(defun create-environment (parameters values)
  "Creates an environment in which parameters are bound to the given values.
It is supposed that the values list is at least as long a the parameters list."
  (let ((m (make-hash-table))
	(vs values))
    (dolist (parameter parameters m)
      (setf (gethash parameter m) (first values))
      (setf values (rest values)))))

(setf env (create-environment '(x y z) '(1 2 1 10)))

(defun get-value (parameter environment)
  (gethash parameter environment))

(get-value 'z env)

(defun testing (e &key print-trace)
  (list e print-trace))

(testing 1)

(testing 1 :print-trace 1)

(subst 'a 1 '(1 2 3 1))
(defun partition-list (lst &optional before after seen-eq)
  "Breaks the given list into parts before and after the = character. The = itself is not included."
  (cond ((null lst) (cons (reverse before) 
			  (reverse after)))
	(seen-eq     (partition-list (cdr lst) 
				     before 
				     (cons (car lst) after) 
				     seen-eq))
	((eq (car lst) '=) (partition-list (cdr lst) 
					   before 
					   after 
					   t))
	(t  (partition-list (cdr lst) 
			    (cons (car lst) before) 
			    after 
			    nil))))

(partition-list '(X Y = Y (X X)))




(defun range-from-zero (last)
  "Returns a list containing all integers from 0 to last, exclusive."
  (let
      ((result nil))
    (dotimes (value last (reverse result))
      (push value result))))

(range-from-zero 5)


(defun break-at (n lst)
  "Splits the given list into two parts, the first one being the first n elements."
  (let ((first nil))
    (dotimes (i n)
      (unless (null lst)
	(push (first lst) first))
      (setf lst (rest lst)))
    (cons (reverse first) lst)))

(break-at 4 (range-from-zero 10))
(break-at 5 '(1 2 3))

;; ---------------------
;; --  SUBSTITUTIONS  --
;; ---------------------

(defun get-value (parameter environment)
  "Get the value bound to the given parameter in the given environment."
  (gethash parameter environment))


(defun create-environment (parameters values)
  "Creates an environment in which parameters are bound to the given values.
It is supposed that the values list is at least as long a the parameters list."
  (let ((m (make-hash-table)))
    (dolist (parameter parameters m)
      (setf (gethash parameter m) (first values))
      (setf values (rest values)))))

;; some simple tests
(defvar env (create-environment '(x y z) '(1 2 1 10)))
(get-value 'z env)



(defun substitute-values (expression parameters env)
  "Substitutes all occurences of the given parameters in the given expression with the
values from the given environment."
  (cond
    ((and (atom expression)
	  (member expression parameters))
     (get-value expression env))
    ((consp expression)
     (cons (substitute-values (car expression) parameters env)
	   (substitute-values (cdr expression) parameters env)))
    (t expression)))


(substitute-values '(x y (x z)) '(x y z) (create-environment '(x y z) '(M (S I) K)))


(defun subst-many (expression parameters values)
  "Binds parameters with given values and substitutes then in the expression.

  i.e. (subst-many '(+ x y z) '(x z) '(1 3)) == (+ 1 y 3)"
  (substitute-values expression parameters (create-environment parameters values)))


(subst-many '(+ x y z) '(x z) '(1 3))
(subst-many '(x y (x z)) '(x y z) '(M (S I) K))
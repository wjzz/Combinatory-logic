(defstruct combinator
  :name
  :parameters
  :body)

(defparameter *rules* (make-hash-table)
  "A lookup table for rewriting rules")

(defun find-combinator (name rule-db)
  "Finds data about the combinator with the given name in the given rule-d_ata-b_ase."
  (gethash name rule-db))

(defun register-combinator (comb rule-db)
  (setf (gethash (combinator-name comb) rule-db) comb))

(defun print-db (rule-db)
  (print rule-db))

(print-db *rules*)
(register-combinator (make-combinator :name 'M) *rules*)



(defun partition-lisp (lst before after seen-eq)
  "Breaks the given list into parts before and after the = character. The = itself is not included."
  (cond ((null lst) (cons (reverse before) (reverse after)))
	(seen-eq     (partition-lisp (cdr lst) before (cons (car lst) after) seen-eq))
	((eq (car lst) '=) (partition-lisp (cdr lst) before after t))
	(t  (partition-lisp (cdr lst) (cons (car lst) before) after nil))))

(defmacro comb (name &rest rest)
  ;; break the rest at '='
  (let*
      ((result (partition-lisp rest '() '() nil))
       (parameters (car result))
       (body       (cdr result))
       (c          (gensym)))    
    `(let ((,c (make-combinator :name       ',name
				:parameters ',parameters
				:body       ',body)))
       (register-combinator ,c *rules*))))

(comb M x = x x)
(comb I x = x)
(print-db *rules*)

;; use-case
; (eval-comb '(M I)) ;; (I I)


(defun create-environment (parameters values)
  "Creates an environment in which parameters are bound to the given values.
It is supposed that the values list is at least as long a the parameters list."
  (let ((m (make-hash-table)))
    (dolist (parameter parameters m)
      (setf (gethash parameter m) (first values))
      (setf values (rest values)))))

(defun get-value (parameter environment)
  "Get the value bound to the given parameter in the given environment."
  (gethash parameter environment))


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

(defun simplify (expression)
  "Simplifes the expression. If the expression is a singleton list we drop the parens"
  (cond ((atom expression) expression)
	((null (cdr expression)) (simplify (car expression)))
	(t (mapcar #'simplify expression))))

(defun rewrite-step (expression rule-db)
  "Makes a single rewrite based on the rule-db. The highest term is rewritten."
  (cond 
    ((atom expression) expression)
    ((consp expression)
     (cond
       ((consp (first expression))
	      (cons (rewrite-step (first expression) rule-db)
		    (rest expression)))
       ((atom (first expression))
     (let*
	 ((combinator (find-combinator (first expression) rule-db))
	  (parameters (combinator-parameters combinator))
	  (values     (rest expression))
	  (body       (combinator-body combinator))
	  (env        (create-environment parameters values)))
       (simplify (substitute-values body parameters env))))))))

(print-db *rules*)

(rewrite-step (rewrite-step '(M I) *rules*) *rules*)

(defun full-rewrite (expression &key print-trace (max-depth 10))
  (when print-trace
     (print expression))
  (unless (= 0 max-depth)  
    (let ((result (rewrite-step expression *rules*)))
      (if (equal expression result)
	  result
	  (full-rewrite result :print-trace print-trace :max-depth (1- max-depth))))))

(full-rewrite '(M (M (M (M (M M))))) :print-trace 1 :max-depth 1)
;(full-rewrite 
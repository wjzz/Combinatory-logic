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

(defun partition-list (lst before after seen-eq)
  "Breaks the given list into parts before and after the = character. The = itself is not included."
  (cond ((null lst) (cons (reverse before) (reverse after)))
	(seen-eq     (partition-list (cdr lst) before (cons (car lst) after) seen-eq))
	((eq (car lst) '=) (partition-list (cdr lst) before after t))
	(t  (partition-list (cdr lst) (cons (car lst) before) after nil))))

(defun range-from-zero (last)
  "Returns a list containing all integers from 0 to last, exclusive."
  (let
      ((result nil))
    (dotimes (value last (reverse result))
      (push value result))))

(range-from-zero 5)

;; We don't want to store variables as symbols starting with letters, so
;; we convert the symbols into integers. This way combinators can be named as letters,
;; according to the tradition.
(defmacro comb (name &rest rest)
  ;; break the rest at '=' and get rid of the symbols used as parameters
  ;; ie. change B x y z = x (y z) into B 0 1 2 = 0 (1 2) 
  (let*
      ((result (partition-list rest '() '() nil))
       (parameters (car result))
       (body       (cdr result))
       (c          (gensym))
       (numbered-params (range-from-zero (length parameters)))
       (numbered-body   (substitute-values body parameters 
					   (create-environment parameters numbered-params))))
    `(let ((,c (make-combinator :name       ',name
				:parameters ',numbered-params ;parameters
				:body       ',numbered-body))) ;body)))
       (register-combinator ,c *rules*))))



(comb T x y z = x z y)
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

;; (('comp A B) X ...) => (A (B x ...))

(defun compositionp (expression)
  (and (consp expression)
       (eq 'comp (first expression))))


(defun rewrite-step (expression rule-db)
  "Makes a single rewrite based on the rule-db. The highest term is rewritten."
  (cond 
    ((atom expression) expression)
    ((consp expression)
     (cond
       ((compositionp (first expression))
	(let* ((comp-expr (first expression))
	       (outer     (second comp-expr))
	       (inner     (third  comp-expr)))
	  (list outer (list inner (rest expression)))))       
	  
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

(print "-----------------")
(full-rewrite '((comp M M) I) :print-trace 1)
(full-rewrite '((comp M M) (comp M M)) :print-trace 1 :max-depth 20)

(full-rewrite '(M (M (M (M (M M)))))   :print-trace 1)
(full-rewrite '((comp (comp M M) M) I) :print-trace 1 :max-depth 20)

(defstruct theorem-struct
  :left
  :right)

(defmacro theorem (name left right)
  `(setf ,name (make-theorem-struct :left ',left
				    :right ',right)))

(macroexpand-1 `(theorem simple I (M M)))

(theorem multi-m
	 I
	 (M I))

(defun prove-theorem-by-rewriting (thm &optional (max-depth 10))
  (let
      ((left  (full-rewrite (theorem-struct-left  thm) :max-depth max-depth))
       (right (full-rewrite (theorem-struct-right thm) :max-depth max-depth)))
    (if (equal left right)
	'success
	nil)))

(prove-theorem-by-rewriting multi-m) ;; OK!
(prove-theorem-by-rewriting (theorem test M I)) ;; OK! (can't be done)

(theorem many-ms I (M (M (M (M (M I))))))
(prove-theorem-by-rewriting many-ms 66) ;; success!



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
       ((consp (first expression))
	(simplify-expression (append (first expression)
				     (rest expression))))

       ; apply simplify to subexpressions and check if simplify changed anything
       (t 
	(let ((result-expression (mapcar #'simplify-expression expression)))
	  (if (equal expression result-expression)
	      expression
	      (simplify-expression result-expression))))))))

;; tests of simplify
(list (equal 'X 
	    (simplify-expression '((((X))))))
     (equal '(A B C D) 
	    (simplify-expression '(((A B) C) D)))
     (equal '(A B (C D E))
	    (simplify-expression '((A B) ((C D) E)))))

(simplify-expression '((A B) C))


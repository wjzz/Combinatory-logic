(load "rewrite.lisp")
(load "sets.lisp")

; (setf *verbose-unit-test* t)

(defun print-all (lst)
  "Prints all elements of the given list, each element on a separate line."
  (dolist (e lst)
    (format t "~a~%" e)))

; (unit-test (print-all '(1 2 3 4)))



(defun all-rewrites-iter (lst seen acc rule-db)
  (loop
     (when (null lst)
       (return acc))
     (let* ((results (all-rewrites (first lst) rule-db))
	    (candidates (mapcar #'(lambda (e) 
				    (append
				     (reverse seen) (cons e (cdr lst))))
				results)))
       (setf acc (append candidates acc)))
     (push (car lst) seen)
     (pop lst)))
	    
       

(defun all-rewrites (expression rule-db)
  "Generates a list of all possible expressions created by unfolding the definition of a single combinator."
  (cond 
    ((atom expression)
     nil)
     ((atom (first expression))
      (multiple-value-bind (e changed)
	  (unfold-combinator expression rule-db)
	(let ((acc (if changed 
		       (list e)
		       nil)))
	  (all-rewrites-iter (rest expression)
			     (list (first expression))
			     acc
			     rule-db))))     
     (t
      (all-rewrites-iter expression nil nil rule-db))))


(defun all-traces (expression rule-db max-depth)
  (defun expand-fringe (new)
    (let ((result st-empty))
      (dolist (e new)
	(let ((expanded (list->st (all-rewrites e rule-db))))
	  (setf result (st-union expanded result))))
      result))
	  
  (defun all-traces-iter (old new depth)
    (if (or (= depth max-depth)
	    (st-empty? new))
	old
	(let*
	    ((expanded-fringe (expand-fringe new))
	     (nnew (st-difference expanded-fringe old))
	     (nold (st-union old nnew)))
	  (all-traces-iter nold nnew (1+ depth)))))
  (all-traces-iter (st-singleton expression) (st-singleton expression) 0))

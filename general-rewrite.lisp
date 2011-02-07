(load "rewrite.lisp")

(setf *verbose-unit-test* t)

(defun print-all (lst)
  "Prints all elements of the given list, each element on a separate line."
  (dolist (e lst)
    (format t "~a~%" e)))

(unit-test (print-all '(1 2 3 4)))



(defun all-rewrites-iter (lst seen acc rule-db)
  (loop
     (when (null lst)
       (return acc))
     (let* ((results (all-rewrites (first lst) rule-db))
	    (candidates (mapcar #'(lambda (e) 
				    (append (reverse seen) (cons e (cdr lst))))
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


  ;; (if (atom expression)
  ;;     nil
  ;;     (when (atom (first expression))
  ;; 	  (multiple-value-bind (e changed) (unfold-combinator expression rule-db)
  ;; 	    (when changed
  ;; 	      (format t "by unfolding: ~a~%" e)
  ;; 	      (push e results))))
  ;; 	(setf results (all-rewrites-iter (rest expression)
  ;; 					 (list (first expression))
  ;; 					 results
  ;; 					 rule-db))
  ;; 	results)))
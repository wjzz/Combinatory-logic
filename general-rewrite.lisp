(load "rewrite.lisp")

(setf *verbose-unit-test* t)

(defun print-all (lst)
  "Prints all elements of the given list, each element on a separate line."
  (dolist (e lst)
    (format t "~a~%" e)))

(unit-test (print-all '(1 2 3 4)))


(defun from-left-to-right (lst)
  (if (atom lst)
      nil
      (let (acc seen)
	(dolist (e lst)
	  (

; (defun 

(defun all-rewrites (comb)
  "Generates a list of all possible combs created by unfolding the definition of a single combinator."
  nil)



(unit-test (print-all (all-rewrites '(M (I I) (I X)))))

(load "reify.lisp")


(defun my-command-line ()
  (or 
   #+SBCL (rest *posix-argv*)
   #+CLISP *args*
   nil))


(reset-def-db)
(rcomb L x y = x (y y))

(defun onlyLLLL (&optional (count-to 5) (max-depth 2))
  (let ((only-Ls 
	 (build-comb X
		     :such-that (X = X X)
		     :search-type first
		     :using (L)
		     :count-to count-to
		     :need-traces t
		     )))
    (unit-test (reify only-Ls :verbose t :max-depth max-depth))))


(let ((args (mapcar #'parse-integer (my-command-line))))
  (eval `(unit-test (onlyLLLL ,@args))))

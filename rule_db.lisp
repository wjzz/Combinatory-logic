(load "combinators.lisp")

(defparameter *rules* (make-hash-table)
  "A lookup table for rewriting rules")

(defun find-combinator (name rule-db)
  "Finds a combinator with the given name in the given rule database."
  (gethash name rule-db))

(defun register-combinator (comb rule-db)
  "Add the given combinator into the given rule database."
  (setf (gethash (combinator-name comb) rule-db) 
	comb))

(defun get-combinators (rule-db)
  "Returns the names of all combinators in the rule database."
  (let ((keys nil))
    (loop for key being the hash-keys of rule-db
	 do (push key keys))
    keys))
    

(defun reset-def-db ()
  (setf *rules* (make-hash-table)))

(defun print-def-db ()
  (print *rules*))


(defmacro rcomb (name &rest rest)
  "Creates a combinator and registers it in the default rule-database"
  `(register-combinator (comb ,name ,@rest) *rules*))


;; TESTS
(register-combinator (make-combinator :name 'M) *rules*)

(print-def-db)

(comb K x y = x)
(rcomb I2 x x = x)
(get-combinators *rules*)
(print-def-db)

(reset-def-db)
(print-def-db)

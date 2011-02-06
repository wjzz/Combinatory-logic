;; a macro for delaying computations
(defmacro delay (expr)
  `(lambda () ,expr))

(defun force (delayed-expr)
  (funcall delayed-expr))



;; TESTS
(defun lazy-test ()
  (format t "Hello!~%")
  1)

(delay (lazy-test))

(force (delay (lazy-test)))
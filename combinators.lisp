(load "helpers.lisp")

(defstruct combinator
  :name
  :parameters
  :body)


;; We don't want to store variables as symbols starting with letters, so
;; we convert the symbols into integers. This way combinators can be named as letters,
;; according to the tradition.

(defmacro comb (name &rest rest)
  "Creates a combinator"
  (let*
      ((result (partition-list rest))
       (parameters (car result))
       (body       (cdr result))
       (numbered-params (range-from-zero (length parameters)))
       (numbered-body   (subst-many body parameters numbered-params)))
    `(make-combinator :name       ',name
		      :parameters ',numbered-params
		      :body       ',numbered-body)))



(comb B x y z = x (y z))
(comb T x y z = x z y)
(comb M x = x x)
(comb I x = x)
(print-db *rules*)
(setf *rules* (make-hash-table))


(defmacro rcomb (name &rest rest)
  "Creates a combinator and registers it in the default rule-database"
  `(register-combinator (comb ,name ,@rest) *rules*))



(rcomb I2 x x = x)
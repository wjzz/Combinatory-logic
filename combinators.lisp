(load "helpers.lisp")

(defstruct combinator
  :name
  :parameters
  :body)


;; We don't want to store variables as symbols starting with letters, so
;; we convert the symbols into integers. This way combinators can be named as letters,
;; according to the tradition and there won't be any conflicts during evalution.

(defmacro comb (name &rest rest)
  "Creates a combinator with a given name and syntax rule.

  ie. (comb T x y z = x (z y)) creates a comb T with 3 parameters."
  (let*
      ((result (partition-list rest))
       (parameters (car result))
       (body       (cdr result))
       (numbered-params (range-from-zero (length parameters)))
       (numbered-body   (subst-many body parameters numbered-params)))
    `(make-combinator :name       ',name
		      :parameters ',numbered-params
		      :body       ',numbered-body)))


;; TESTS
(comb B x y z = x (y z))
(comb T x y z = x z y)
(comb M x = x x)
(comb I x = x)

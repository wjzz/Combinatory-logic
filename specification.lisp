(load "theorems.lisp")

(defstruct specification
  :parameters
  :body
  :combinators)

(defmacro spec (from obtain using)
  `(make-specification
    :parameters  ',from
    :body        ',obtain
    :combinators ',using))

; (defvar test-spec '(spec (x y z) (x (y z)) (B)))
;(macroexpand-1 test-spec)
;(eval test-spec)

(defun build-theorem (comb spec)
  (make-theorem-struct :left  (specification-body spec)
		       :right (cons comb (specification-parameters spec))))


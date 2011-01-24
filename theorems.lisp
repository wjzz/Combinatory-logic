(load "rewrite.lisp")

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


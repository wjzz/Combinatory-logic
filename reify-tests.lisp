(load "reify.lisp")

(load "example-combinators.lisp")

(let ((spec (express I :using (S K) :count-from 4 :count-to 5)))
  (reify spec))

(unit-test (reify (build-comb X :such-that (X Y = X) :using (S K))))

;; (setf B (spec (x y z) (x (y z)) (B)))
;; ; (reify B)


;; (setf D (spec (x y z w)
;; 	     (x y (z w))
;; 	     (B)))

;; ; (reify D)


;; (setf triple (spec (x y z w)
;; 		   (x (y z w))
;; 		   (B)))
;; ; (reify triple)


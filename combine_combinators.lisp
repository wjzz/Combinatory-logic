(load "theorems.lisp")

;; TODO  define macros

(defmacro spec (name &rest rest)
  ...)

(defmacro reify (name &rest rest)
  ...)


;; use case
(defvar D (spec D x y z = x z y))

;; now we want to be able to generate such D by combining other combinators
(reify D 
       using B M I 
       in 100 steps) ;; generate a D by applying only B, M and I
;; returns nil if we werent able to find one

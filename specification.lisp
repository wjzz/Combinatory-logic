(load "equality.lisp")

(defstruct specification
  :comb-name
  :equality
  :using
  :search-type
  :count-from
  :count-to)
  

(defun such-that->equality (lst)
  (let ((parts (partition-list lst)))
    (make-equality-struct :left (car parts)
			  :right (cdr parts))))



(defmacro build-comb (comb-name &key such-that using (search-type 'all) (count-from 1) (count-to 3))
  `(make-specification 
    :comb-name ',comb-name
    :equality (such-that->equality ',such-that)
    :using ',using
    :search-type ',search-type
    :count-from ,count-from
    :count-to   ,count-to    
    ))



(defmacro express (comb-name &key using (search-type 'all) (count-from 1) (count-to 3))
  (let*
      ;; what happens if comb is not found?
      ((comb (find-combinator comb-name (get-def-db)))
       (left (cons comb-name (combinator-parameters comb)))
       (right (combinator-body comb)))
    `(build-comb ,comb-name
		 :such-that (,left = ,right)
		 :using ,using
		 :search-type ,search-type
		 :count-from ,count-from
		 :count-to ,count-to
		 )))


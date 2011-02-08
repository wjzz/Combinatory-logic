(load "helpers.lisp")

(defun list->st (lst)
  (remove-duplicates lst :test #'equal))

(defun st-member (elem lst &key (test #'equal))
  (member elem lst :test test))

(defun st-union (set1 set2)
  (union set1 set2))

(defun st-nunion (set1 set2)
  "May destroy set1."
  (nunion set1 set2))

(defun st-difference (set1 set2)
  (set-difference set1 set2))

(defun st-empty? (set)
  (null set))

(defparameter st-empty ()
  nil)
    
(defun st-singleton (elem)
  (list elem))


; (setf *verbose-unit-test* t)

(unit-test (list->st '(a b (c c) (c c))))

(unit-test (st-member 'a (list->st '(a b c))))
(unit-test (st-member 'a (list->st '(b c))))

(unit-test (st-union (list->st '(a b))
		     (list->st '(b c))))


(unit-test (st-difference (list->st '(a b c d))
			  (list->st '(b c))))


(unit-test (st-empty? st-empty))
(unit-test (st-empty? (list->st '(1 2 3))))

(unit-test (st-empty? (st-singleton 1)))
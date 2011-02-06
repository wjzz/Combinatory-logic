(load "helpers.lisp")


(defun combine-results (xs ys acc)
  "Generate all possible pairs and append them to acc."
  (let ((result acc))
    (dolist (x xs)
      (dolist (y ys)
	(push (cons x y) result)))
    result))

(unit-test (combine-results '(1 2) '(a b c) '(start)))

; ----------------
; GENERATE TREES -
; ----------------

(defun generate-less-mem (count available-combs table)
  (let ((result (gethash count table)))
    (if result
	result
	(let ((new-result (generate-less-mem-iter count available-combs table)))
	  (setf (gethash count table) new-result)
	  new-result))))

(defun generate-less-mem-iter (count available-combs table)
  (if (= 1 count)
      available-combs
      (let (results)
	(dotimes (n (1- count))
	  (let ((left  (generate-less-mem (1+ n)        available-combs table))
		(right (generate-less-mem (- count n 1) available-combs table)))
	     (setf results (combine-results left right results))))
	results)))



(defun generate-combs (count available-combs)
  "Generate all trees with exactly count leaves, with leaves being the elements of available-combs."
  (let ((table (make-hash-table)))
    (generate-less-mem-iter count available-combs table)))

(unit-test (generate-combs 2 '(M B I)))

; --------------------------
; CONVERT TREE TO COMBINATOR
; --------------------------

(defun tree->nested-lists-iter (tree)
  (if (atom tree)
      tree
      (let ((ncar (tree->nested-lists-iter (car tree)))
	    (ncdr (tree->nested-lists-iter (cdr tree))))
	(list ncar ncdr))))

(defun tree->nested-lists (tree)
  (simplify-expression (tree->nested-lists-iter tree)))


(unit-test (tree->nested-lists '(M . I)))
(unit-test (tree->nested-lists 'I))
(unit-test (tree->nested-lists '((M . S) . I)))
(unit-test (tree->nested-lists '(I . (M . S))))
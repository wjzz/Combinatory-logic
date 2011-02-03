(load "combine_combinators.lisp")


;; we try to add memoization
(defvar *table*)

(defun reset-table ()
  (setf *table* (make-hash-table :test #'equal)))

(reset-table)
;*table*

;(setf (gethash (cons 1 '(M)) *table*) '(M))


(defun generate-combs-mem (count available-combs)
  (let ((query-result (gethash count *table*)))
    (if query-result
	query-result
	(let ((new-result (generate-combs count available-combs)))
	  (setf (gethash count *table*) new-result)
	  new-result))))

(defun generate-combs (count available-combs)
  (cond ((= 1 count) available-combs)
	((< 1 count) 
	 (let ((parts (partitions count)))
	   (mappend (lambda (partition)
		      (cart-prod (mapcar (lambda (cnt)
					   (generate-combs-mem cnt available-combs))
					 partition)))
		    parts)))))
	   

 
;(reify (spec (x) (x) (S K)) 7)

(defun display-combs (max-level combs)
  (dotimes (n max-level)
    (format t "~d ~d ~%" n (generate-combs-mem n combs))))

; (display-combs 5 '(B))

(defun count-combs (max-level combs)
  (dotimes (n max-level)
    (format t "~d ~d ~%" n (length (generate-combs-mem n combs)))))


(count-combs 13 '(B))
;(count-combs 11 '(B))
;*table*
;(reset-table)
;(count-combs 11 '(B)) ;; can only go upto 8 ~4k


;(count-combs 10 '(I M)) ;; can only go upto 6 ~12k

; (reduce #'append '((1 2) (4) (5 6)) :initial-value nil)



;(reset-table)
;(generate-combs-mem 5 '(M I))
;*table*


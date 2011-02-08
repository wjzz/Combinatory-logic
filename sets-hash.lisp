(load "helpers.lisp")

; ok
(defun list->hst (lst)
  (let ((result (hst-empty)))
    (dolist (x lst)
      (setf (gethash x result) t))
    result))
 
; ok
(defun hst-member (elem set &key (test #'equal))
  (gethash elem set))

(defun hst-union-with-list! (set lst)
  (dolist (e lst)
    (setf (gethash e set) t))
  set)
      
(defun hst-difference! (set1 set2)
  "Will destroy set1"
  (loop for key being the hash-keys of set2
        do (remhash key set1))
  set1)

(defun hst-union! (set1 set2)
  "Will destroy set1"
  (loop for key being the hash-keys of set2
       do (setf (geshash key set1) t))
  set1)

(defun hst-empty? (set)
   (zerop (hash-table-count set)))

(defun hst-empty ()
  (make-hash-table :test #'equal))
    
(defun hst-singleton (elem)
  (list->hst (list elem)))


; (setf *verbose-unit-test* t)


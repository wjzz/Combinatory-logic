(load "lazy.lisp")

;;; we represent a stream as a cons of a value and a stream

(defmacro stream-cons (head tail)
  "Creates a stream with given head and tail. The tail is not evaluated."
  `(cons ,head (delay ,tail)))

(macroexpand-1 `(stream-cons 1 (/ 1 0)))


(defconstant stream-empty
  nil
  "An empty stream.")

(defun stream-emptyp (str)
  "Returns t if the given stream is empty, nil otherwise."
  (null str))



(defun stream-head (stream)
  "Returns the first element [the head] of the given stream.
  If the stream is empty, an exception is thrown."
  (if (stream-emptyp stream)
      (error "Stream-head: empty stream")
      (first stream)))

(defun stream-tail (stream)
  "Returns the given stream without the first element.
   If the stream is empty, an exception is thrown."
  (if (stream-emptyp stream)
      (error "stream-tail: empty stream")
      (force (rest stream))))

;; (stream-head stream-empty)

(defun stream-from-list (lst)
  "Creates a stream based on the given list"
  (if (null lst)
      stream-empty
      (stream-cons (first lst)
		   (stream-from-list (rest lst)))))

(defun stream-to-list (stream)
  "Converts the given stream into a list.
   This means forcing all the delayed computations."
  (if (stream-emptyp stream)
      nil
      (cons (stream-head stream)
	    (stream-to-list (stream-tail stream)))))

(stream-to-list (stream-from-list '(1 2 3)))



(defun stream-from-to (lower upper)
  "Returns a stream containing values from lower to upper (inclusively)."
  (if (> lower upper)
      stream-empty
      (stream-cons lower (stream-from-to (1+ lower) upper))))

(stream-to-list (stream-from-to 1 10))



(defun stream-map (func stream)
  "Creates a new stream from the given stream by applying the given function to all elements."
  (if (stream-emptyp stream)
      stream-empty
      (stream-cons (funcall func (stream-head stream))
		   (stream-map func (stream-tail stream)))))

(stream-to-list (stream-map (lambda (x) (* x x))
			    (stream-from-to 1 10)))



(defun stream-concat (stream1 stream2)
  "Concatenates (merge) the given streams."
  (if (stream-emptyp stream1)
      stream2
      (stream-cons (stream-head stream1)
		   (stream-concat (stream-tail stream1)
				  stream2))))

(setf str1 (stream-map (lambda (x) (format t "Evaluating ~d~%" x) x)
		       (stream-from-to 1 5)))

(setf str2 (stream-from-to 6 10))
			       

(stream-to-list (stream-concat str1 str2))


(defun stream-concat-map (gen-func stream)
  "Applies the given function to all elements of the given stream
   and concatenates the results."
  (if (stream-emptyp stream)
      stream-empty
      (let ((fst (funcall gen-func (stream-head stream))))
	(stream-concat fst 
		       (stream-concat-map gen-func (stream-tail stream))))))

(setf test-concat-map (stream-concat-map (lambda (x) (stream-from-to 1 x))
					 (stream-from-to 1 5)))

(stream-to-list test-concat-map)



;; We try to implement a part of Haskell's way of emulatin backtracking by lazy lists
;; The goal of stream-gen is to allow one to write nice-looking list manipulation code

;; TODO add a default case to allow the user to use if's for example?
(defmacro stream-gen (fst &rest body)
  (case (first fst)
    (yield
     `(stream-cons ,(second fst) stream-empty))
    (choose
     (let
	 ((var (second fst))
	  (lst (third  fst))
	  (n-fst (first body))
	  (n-body (rest body)))
       `(stream-concat-map (lambda (,var) (stream-gen ,n-fst ,@n-body))
			   ,lst)))))
	 

(let ((x 1))
  (stream-gen (yield (+ x 3))))

(stream-to-list (stream-gen
		    (choose x (stream-from-to 1 4))
		    (choose y (stream-from-list '(a b c)))
		    (yield (cons x y))))

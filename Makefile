all:
	sbcl --script reify-tests.lisp 2>/dev/null
test:
	sbcl --script reify-tests.lisp

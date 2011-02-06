(load "rule_db.lisp")

; Some combinators 
(reset-def-db)

(rcomb M x = x x)
(rcomb I x = x)
(rcomb K x y = x)
(rcomb B x y z = x (y z))
(rcomb T x y z = x (z y))
(rcomb S x y z = x z (y z))
(get-combinators *rules*)

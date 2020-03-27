#lang typed/racket/base #:transient
(require typed/json)
(: jsx JSExpr)
(define jsx #hasheq((a . "val1") (b . "val2") (c . "val3")))

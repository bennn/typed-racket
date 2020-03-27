#lang typed-scheme #:transient
(require scheme/match)
(ann (match '(a b c)
       [(list sym more ...) 1]
       [else 1]) Integer)

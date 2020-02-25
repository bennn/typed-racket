#lang typed/racket/base

(provide quad)

;; quad wants to be generic
;; if it's a function, it must impose a type on its output value
;; whereas if it's syntax, it can avoid demanding or imposing any typing
;;bg worried I can't keep the syntax
(define-syntax-rule (quad name attrs items)
  (list* name attrs items))

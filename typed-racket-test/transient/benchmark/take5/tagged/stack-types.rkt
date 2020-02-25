#lang typed/racket/base #:transient

(require "card-adapted.rkt")
(provide Stack)
(define-type Stack
  (Listof Card))

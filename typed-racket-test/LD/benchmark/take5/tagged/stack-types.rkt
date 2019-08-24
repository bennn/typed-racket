#lang typed/racket/base #:locally-defensive

(require "card-adapted.rkt")
(provide Stack)
(define-type Stack
  (Listof Card))

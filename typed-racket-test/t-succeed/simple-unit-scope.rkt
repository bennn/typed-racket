#lang typed/racket #:transient

(define-signature y^ ([y : Integer]))

(let ((y 1))
  (unit (import) (export y^)
        (define y 2)))

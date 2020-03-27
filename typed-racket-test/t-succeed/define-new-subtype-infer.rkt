#lang typed/racket #:transient

(define-new-subtype T (make-T (List Integer)))

(: f : T -> Integer)
(define (f t)
  (define x (car t))
  (* x 2))

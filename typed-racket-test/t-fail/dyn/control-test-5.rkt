#;
(exn-pred exn:fail:contract? #rx"string-append")
#lang racket/load

;;bg transient does not catch the error; apply sends a bad value to string-append

;; check typed-untyped interaction with cont marks

(module typed typed/racket #:transient
  (provide key f)

  (: key (Continuation-Mark-Keyof String))
  (define key (make-continuation-mark-key))

  (: f (-> String))
  (define (f)
    (apply string-append
           (continuation-mark-set->list
            (current-continuation-marks)
            key))))

(module untyped racket
  (require 'typed)

  (with-continuation-mark
   key 'hello ; should be string
   (f)))

(require 'untyped)


#;
(exn-pred exn:fail?)
#lang typed/racket #:transient

(let ([x #f])
  (with-asserts ([x])
                x))

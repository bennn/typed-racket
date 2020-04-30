#;
(exn-pred exn:fail:contract? #rx"transient-assert")
#lang racket/load

(module outer typed/racket #:transient
  (: f : Integer -> Integer)
  (define (f x) (add1 x))
  
  (provide f)
  
  (module* m racket
    (require (submod ".."))
    (f "foo"))
  
  (module* main racket
    (require (submod ".." m))))

(require (submod 'outer main))

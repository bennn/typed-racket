#lang typed/racket/base #:transient
;; Iteratee
(define-type (Alias A D) (U #f (Main A D)))

(struct: (A D) Main
  ([resume : (Alias A D)])) ;

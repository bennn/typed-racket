#lang typed/racket/base #:transient
(with-handlers* ([exn:fail? (λ (e) 2)])
  1)
(with-handlers* ([exn:fail? (λ (e) 2)])
  (error "test"))

#lang typed/racket/base #:transient

(define (run-ground-vm)
  (with-handlers ([exn:break? void])
    (void)))

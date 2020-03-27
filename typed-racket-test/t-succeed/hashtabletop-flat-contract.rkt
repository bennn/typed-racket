#lang typed/racket/base #:transient

;; Test that `HashTableTop` generates a flat contract

(define h : HashTableTop (hash))
(void (cast h HashTableTop))

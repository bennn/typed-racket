#lang typed/racket/base #:transient

(require typed/racket/unit)

(define-syntax-rule (define* form ...) (define form ...))

(define-signature sig^
  ([id : (Any → Any)]))

(define-unit sig@
  (import)
  (export sig^)

  (define* (id [x : Any]) : Any x))

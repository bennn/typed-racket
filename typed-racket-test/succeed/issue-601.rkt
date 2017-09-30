#lang racket

(module class-helpers typed/racket

  (provide look-in-struct
  (struct-out my-struct))

  ;; PROBLEM SPECIFIC TO 'define-struct':
  (define-struct my-struct ([a : Natural]))
  ;(struct my-struct ([a : Natural]) #:extra-constructor-name make-my-struct)

  (: look-in-struct (my-struct -> Natural))
  (define (look-in-struct s)
  (my-struct-a s))

)

(require 'class-helpers rackunit)

(check-exn exn:fail:contract?
  (λ ()
    ;; SHOULD FAIL: not a natural...
    (define info (my-struct "not a natural..."))
    (look-in-struct info)))

#lang racket/base

(require scribble/example)

(for ((the-eval (in-list (list
                           (make-base-eval #:lang 'typed/racket)
                           (make-base-eval '(require typed/racket))))))
  (examples
    #:label #f #:eval the-eval
    (eval:error (define-predicate p? (All (A) (Listof A))))
    (require/typed racket/base
      [object-name (-> (U Struct-Type-Property Regexp)
                       (U String Bytes Symbol))])
    (object-name #rx"a regexp")))


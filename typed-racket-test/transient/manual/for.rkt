#lang typed/racket/base #:transient

;; For combinators don't need a codomain check,
;;  or a domain check on the `for-loop` functions
;;
;; Expand, look for:
;; - no "dynamic-typecheck" around the final result
;; - no domain "dynamic-typecheck" for any functions
;; - no dynamic-typecheck around unsafe-cdr

(void?
  (for ((x (in-list '(A B C))))
    x))

(list?
  (for/list : (Listof Symbol) ((x '(A B C)))
    x))

(list?
  (for/fold
            ((acc : (Listof Symbol) '()))
            ((x (in-list '(A B C))))
    (cons x acc)))


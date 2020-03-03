#lang typed/racket/base #:transient

;; For combinators don't need a codomain check
;;
;; Expand, look for "dynamic-typecheck" around the final result --- should be nothing

;;(require (only-in racket/file file->value))
;;(define-predicate freq-list? (Listof (List String Integer)))
;;(: file->words (-> String (Listof String)))
;;(define (file->words filename)
;;  (define words+freqs (file->value (string->path filename)))
;;  (unless (freq-list? words+freqs) (error "expected a frequency list"))
;;  (for/list : (Listof String) ([word+freq : (List String Integer) words+freqs])
;;    (car word+freq)))


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


#lang typed/racket/base #:transient

;; Expand, and make sure no transient checks,
;;  because cdr applied to a list is OK

(require racket/list racket/unsafe/ops)

(define x0 : (Listof Symbol) '(A B C))
(define x1 : (List Symbol Symbol) '(A B))

(cdr x0)
(cdr x1)
(unsafe-cdr x0)
(unsafe-cdr x1)

;(rest (ann '(A . X) (Pairof Symbol Symbol)))

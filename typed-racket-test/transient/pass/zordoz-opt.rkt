#lang typed/racket/base #:transient

;; Originally raised error in optimizer,
;;  expected kernel-literal (or something) got gensym var

(require require-typed-check
         racket/match)

(define-type result Symbol)
(define-type zo Symbol)
(define zo? symbol?)

(define-type Context (U zo (Listof zo) (Listof result)))
(define-type History (Listof Context))
(define-type History* (Listof History))

(: find (-> String Context History History* (Values Context History History*)))
(define (find raw ctx hist pre-hist)
  (define arg "hello")
  (cond [(and arg (zo? ctx))
         (define results (my-find ctx arg))
         (match results
           ['()
            (values ctx hist pre-hist)]
           [_
            (values ctx '() pre-hist)])]
        [else
         (values ctx hist pre-hist)]))

(: my-find (-> zo String [#:limit (U Natural #f)] (Listof result)))
(define (my-find z str #:limit [lim #f])
  '())


#lang typed/racket/base #:transient

;; Example from vss-popl-2017
;;  inner function causes the error,
;;  but blame map deduces that cast is the problem

(module t typed/racket/base #:transient
  (: makeEqChecker (-> String (-> String Boolean)))
  (define (makeEqChecker v)
    (: eqChecker (-> String Boolean))
    (define (eqChecker w)
      (string=? v w))
    eqChecker)

  (provide makeEqChecker))

(require/typed 't
  ((makeEqChecker castFunc) (-> String (-> Any Any))))

((castFunc "Hi") 42)

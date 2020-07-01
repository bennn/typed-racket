#lang typed/racket #:transient

(module uuu racket/base
  (struct aaa [bbb] #:transparent)
  (define (make-aaa)
    (aaa (list "not symbol")))
  (provide (struct-out aaa) make-aaa))

(require/typed 'uuu
  (#:struct aaa ([bbb : (Listof Symbol)]))
  (make-aaa (-> aaa)))

(define a (make-aaa))

(match a
  ((aaa sym*)
   (car sym*))
  (_
    #f))

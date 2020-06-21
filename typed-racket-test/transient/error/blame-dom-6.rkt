#lang racket/base

;; Export function,
;; error in opt keyword arg,
;; blame 1 boundary

(module t typed/racket/base #:transient
  (: fff (->* [String] [Any #:xxx Symbol] Void))
  (define (fff x #:xxx [xxx 'xxx] [y 'yyy])
    (symbol->string xxx)
    (void))
  (provide fff))

(require 't)
(fff "sym" #:xxx "not-sym" 'not-str)


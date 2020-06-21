#lang racket/base

;; Export function,
;; error in opt keyword arg,
;; blame 1 boundary

(module t typed/racket/base #:transient
  (: fff (->* [Void #:aaa String] [Any #:zzz Symbol] Void))
  (define (fff x #:aaa aaa #:zzz [zzz 'xxx] [y 'yyy])
    (symbol->string zzz)
    (void))
  (provide fff))

(require 't)
(fff (void) #:aaa "aaa" #:zzz "not-sym" 'not-str)


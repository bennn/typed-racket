#lang racket/base

;; Similar to blame-dom-7 but alpha-swap keyword names

(module t typed/racket/base #:transient
  (: fff (->* [Void #:zzz String] [Any #:aaa Symbol] Void))
  (define (fff x #:zzz zzz #:aaa [aaa 'xxx] [y 'yyy])
    (symbol->string aaa)
    (void))
  (provide fff))

(require 't)
(fff (void) #:zzz "zzz" #:aaa "not-sym" 'not-str)


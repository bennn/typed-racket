#lang racket/base

;; Multiple mandatory keywords, error in middle,
;; blame 1 boundary

(module t typed/racket/base #:transient
  (: fff (->* [Void #:aaa String #:bbb String #:ccc Symbol #:ddd String] [] Void))
  (define (fff x #:aaa aaa #:bbb bbb #:ccc ccc #:ddd ddd)
    (symbol->string ccc)
    (void))
  (provide fff))

(require 't)
(fff (void) #:ccc "ccc" #:ddd "ddd" #:aaa "aaa" #:bbb "bbb")


#lang racket/base

(module ttt typed/racket/base #:transient
  (require racket/set)
  (: fff (-> (Setof Symbol) Void))
  (define (fff sss)
    (define s (set-first sss))
    (void))
  (provide fff))

(require 'ttt racket/set)

(fff (list->set '("A" "B")))

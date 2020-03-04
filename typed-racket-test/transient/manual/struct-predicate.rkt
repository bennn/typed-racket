#lang typed/racket/base #:transient

;; Struct predicates don't need a typecheck
;;
;; Expand this file,
;;  make sure there are no transient-assert s

(module a racket/base
  (provide (struct-out a))
  (struct a [x]))

(require/typed 'a
  (#:struct a ([x : Symbol])))

(struct b ((y : Symbol)))

(a? #f) ;; require/typed struct
(b? #f) ;; TR struct
(exn? #f) ;; builtin struct

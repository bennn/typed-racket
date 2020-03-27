#lang typed/racket/base #:transient

;; Test for GH issue 304
;;
;; Make sure polymorphic #:type-name structs can be exported

(struct (A) foo () #:type-name Foo)
(provide (struct-out foo))

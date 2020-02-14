#lang typed/racket #:locally-defensive

;; Identifier imported from untyped-contract should have original type,
;;  not the type for the refined contract

;; HANG ON this test must fail, because Transient needs a contract
;;  and that contract should be the same as untyped --- because Transient is
;;  a free pipeline to untyped

;(require "untyped-contract-aux/untyped.rkt")
;
;(define (g (z : (U Symbol String))) : Void
;  (f z))


#lang typed/racket #:transient

;; Test for PR 14355. Ideally would be a unit test, but
;; the bug could not be triggered via the harness

;; make sure this doesn't result in an internal error
(cast (cast "x" String) String)


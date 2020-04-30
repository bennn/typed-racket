#;
(exn:pred exn:fail:contract? #rx"broke its own contract")
#lang typed/racket #:transient
(require/typed racket/base [list (All (a) Float)])
(* 3.3 list)


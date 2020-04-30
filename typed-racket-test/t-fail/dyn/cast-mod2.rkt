#;
(exn-pred exn:fail:contract? #rx"transient-assert" )

#lang typed/racket/base #:transient

((cast (lambda () 3) (-> String)))


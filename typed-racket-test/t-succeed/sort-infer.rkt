#lang typed/racket #:transient
(define ss '("one" "two" "three")) ; (Listof String)
(sort ss string<?)

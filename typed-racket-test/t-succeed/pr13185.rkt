#lang typed/racket #:transient
(for/vector: : (Vectorof Natural) ([i : Natural (in-range 20)]) i)

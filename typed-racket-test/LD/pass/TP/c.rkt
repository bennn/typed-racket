#lang typed/racket/base #:locally-defensive
(require "a.rkt" "d.rkt")
(require/typed "b.rkt" (bbx (Boxof (Boxof Integer))))
g
(f bbx)

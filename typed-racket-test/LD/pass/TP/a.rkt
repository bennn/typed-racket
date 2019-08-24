#lang typed/racket/base
        (provide f)
        (define (f (x : (Boxof (Boxof Integer))))
          (unbox (unbox x)))

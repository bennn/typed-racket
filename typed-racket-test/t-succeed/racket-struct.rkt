#lang typed/racket #:transient

(struct: x ([y : Number]))

(x 1)
(x-y (x 7))
(ann x? (Any -> Boolean : x))

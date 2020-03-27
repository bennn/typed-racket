#lang typed/racket #:transient

(ann (for ([#{i : Integer} '(1 2 3)]) (display i)) Void)

#lang typed/racket #:transient
(ann (min 255 (max 0 (ann 0 Integer))) Byte)

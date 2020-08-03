#lang typed/racket/base #:transient

(require/typed racket/base
 (values (-> Any Any : String)))

(define x : Any 0)

(define fake-str : String
  (if (values x)
    (ann x String)
    (error 'unreachable)))

(string-length fake-str)


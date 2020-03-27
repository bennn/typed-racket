#lang typed/racket #:transient
(require/typed racket
               (current-directory (-> Path)))

(unless (equal? (parameter? current-directory)
                (if (parameter? current-directory) #t #f))
  (error 'unsound!))

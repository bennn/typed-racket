#lang typed/scheme #:transient

(require typed/test-engine/scheme-tests)

(check-expect 4 4)
(check-expect 3 4)

(check-random (random 10) (random 10))
(check-random (random 10) 12)

;(check-satisfied 0 zero?)
;(check-satisfied 1 zero?)

(check-within 0 0 1)
(check-within 3 0 1)

;(check-error (raise-user-error 'die))
;(check-error 'ok)

(check-error (raise-user-error "die") "die")
(check-error (raise-user-error 'die) "hello")

(check-member-of 0 2 1 0)
(check-member-of 0 2 1 3)

(check-range 3 0 5)
(check-range 5 1 2)

(test)

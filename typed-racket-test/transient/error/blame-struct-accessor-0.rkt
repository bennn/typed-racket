#lang typed/racket/base #:transient

(module uuu racket/base
  (struct posn [x y])
  (define apoint (posn "A" "B"))
  (provide apoint (struct-out posn)))

(require/typed 'uuu
  (#:struct posn ((x : Symbol) (y : Symbol)))
  (apoint posn))

(posn-x apoint)

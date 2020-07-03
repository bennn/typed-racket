#lang typed/racket/base #:transient

;; Blame 1 boundary,
;;  call-with-values returns bad result

(module u racket/base
  (provide abc)
  (define abc (cons 'A (cons "B" #"C"))))

(require/typed 'u
  (abc (Pairof Symbol (Pairof Void String))))

(cadr abc)


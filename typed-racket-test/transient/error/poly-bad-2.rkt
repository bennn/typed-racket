#lang typed/racket/base #:transient

;; Expected: transient-assert error
;;
;; To remove the error, we need to treat `inst` as an elimination form
;; when the poly type does not guarantee some kind of shape.

(require/typed racket/base
  (cdr (All (A) (-> A A))))

(let ((v : (Pairof String String)
       ((inst cdr (Pairof String String)) '("A" . "B"))))
  (car v))

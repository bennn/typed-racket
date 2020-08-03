#lang typed/racket/base #:transient

;; Expected: contract generation error
;;
;; To remove the error, we need to treat `inst` as an elimination form
;; when the poly type does not guarantee some kind of shape.

(require/typed racket/base
  (cdr (All (A) (U (Boxof A) (Pairof A A) (-> String String)))))

(let ((v : (U (Boxof String) (Pairof String String) (-> String String))
       (inst cdr String)))
  (if (box? v)
    #f
    (if (pair? v)
      #f
      (v "aaa"))))


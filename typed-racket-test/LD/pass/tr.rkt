#lang typed/racket

(: x String)
(define x "hello")

(: f (-> (Sequenceof #t) Void))
(define (f xs)
  ;(let ((x : Integer 0))
  (for ([x xs])
    (void)))
  ;)


;; FIRST try to keep tc-app-special fixed and change "type-annotation"
;; SECOND maybe change the question

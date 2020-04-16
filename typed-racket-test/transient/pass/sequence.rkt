#lang typed/racket/base ; #:transient

(define (f0 (tm : (Listof Exact-Nonnegative-Integer)))
  (for/list : (Listof String)
            ([n : Exact-Nonnegative-Integer tm])
    (number->string n)))
(void (f0 '("A" "B")))

(define (f1 (fields : (Sequenceof (Pair String (-> String)))))
  (for/list : (Listof String)
            ([fd : (Pair String (-> String)) fields])
    "hi"))
(void (f1 (list (cons "A" (lambda () "B")))))

(: f2 (All (A) (-> (-> A String) (Vectorof A) String)))
(define (f2 k xs)
  (apply string-append
    (for/list : (Listof String)
              ([x : A xs])
      (k x))))
(void (f2 '#("A" "B")))

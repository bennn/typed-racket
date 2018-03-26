#lang typed/racket/base
(require racket/vector racket/unsafe/ops)

(: mvec-prop (All (A) (-> (Vectorof A) (Mutable-Vectorof A))))
(: ihash-prop (All (A B) (-> (HashTable A B) A (Immutable-HashTable A B))))
(: mhash-prop (All (A B) (-> (HashTable A B) A (U (Mutable-HashTable A B) (Weak-HashTable A B)))))

(define (mvec-prop v)
  (define x (random 4))
  (define a (vector-ref v 0))
  (cond
    [(= x 0)
     (vector-fill! v a)
     v]
    [(= x 1)
     (vector-set! v 0 a)
     v]
    [(= x 2)
     (unsafe-vector-set! v 0 a)
     v]
    [(= x 3)
     (vector-copy! v 0 '#() 0)
     v]
    [else (error 'never-happens)]))

(define (ihash-prop h k)
  (define x (random 10))
  (define v (hash-ref h k))
  (cond
    [(= x 0)
     (hash-set h k v)
     h]
    [else
     (error 'never-happens)]))

(define (mhash-prop h k)
  (define x (random 2))
  (define v (hash-ref h k))
  (cond
    [(= x 0)
     (hash-set! h k v)
     h]
    [(= x 0)
     (hash-clear! h)
     h]
    [else
      (error 'never-happens)]))


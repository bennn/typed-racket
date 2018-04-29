#lang racket/base

;; Static contracts for common data types.
;; These are used during optimizations as simplifications.
;; Ex: (listof/sc any/sc) => list?/sc

(require "simple.rkt" "any.rkt"
         (for-template racket/base racket/list racket/set racket/promise
                       racket/class racket/unit racket/async-channel))
(provide (all-defined-out))

(define identifier?/sc (flat/sc #'identifier?))
(define box?/sc (flat/sc #'box?))
(define syntax?/sc (flat/sc #'syntax?))
(define promise?/sc (flat/sc #'promise?))

(define cons?/sc (flat/sc #'cons?))
(define list?/sc (flat/sc #'list?))

(define mpair?/sc (flat/sc #'mpair?))

(define set?/sc (flat/sc #'set?))
(define vector?/sc (flat/sc #'vector?))

(define hash?/sc (flat/sc #'hash?))

(define sequence?/sc (flat/sc #'sequence?))

(define channel?/sc (flat/sc #'channel?))
(define async-channel?/sc (flat/sc #'async-channel?))
(define thread-cell?/sc (flat/sc #'thread-cell?))
(define prompt-tag?/sc (flat/sc #'continuation-prompt-tag?))
(define continuation-mark-key?/sc (flat/sc #'continuation-mark-key?))
(define evt?/sc (flat/sc #'evt?))

(define class?/sc (flat/sc #'class?))
(define unit?/sc (flat/sc #'unit?))

(define struct-type?/sc (flat/sc #'struct-type?))

(define procedure?/sc (flat/sc #'procedure?))
(define parameter?/sc (flat/sc #'parameter?))

(define (procedure-arity-includes/sc n kws-ok)
  (flat/sc #`(λ (f) (procedure-arity-includes? f '#,n '#,kws-ok))))

(define (procedure-mandatory-keywords/sc pre-kws)
  (define kws (sort pre-kws keyword<?))
  (if (null? kws)
    any/sc
    (flat/sc
      #`(λ (f)
          (let-values ([(mand-kws _) (procedure-keywords f)])
            (equal? mand-kws '#,kws))))))

(define (procedure-optional-keywords/sc pre-kws)
  (define kws (sort pre-kws keyword<?))
  (if (null? kws)
    any/sc
    (flat/sc
      #`(λ (f)
          (let-values ([(_ opt-kws) (procedure-keywords f)])
            ;; Goal: "expected" \subseteq "actual"
            (let loop ([expected-kws '#,kws]
                       [actual-kws opt-kws])
              (cond
               [(null? expected-kws)
                #true]
               [(or (null? actual-kws) (keyword<? (car expected-kws) (car actual-kws)))
                #false]
               [(keyword<? (car actual-kws) (car expected-kws))
                (loop expected-kws (cdr actual-kws))]
               [else
                (loop (cdr expected-kws) (cdr actual-kws))])))))))

(define (make-procedure-arity-flat/sc num-mand mand-kws opt-kws)
  (flat/sc
    #`(λ (f)
        (and (procedure? f)
             (procedure-arity-includes? f '#,num-mand '#,(not (null? mand-kws)))
             #,@(if (null? mand-kws)
                  #'()
                  #`((let-values (((f-mand-kws _) (procedure-keywords f)))
                       (equal? '#,mand-kws f-mand-kws))))
             #,@(if (null? opt-kws)
                  #'()
                  #`((let-values (((_ f-opt-kws) (procedure-keywords f)))
                       (let loop ((expected-kws '#,(sort opt-kws keyword<?))
                                  (actual-kws f-opt-kws))
                         (cond
                          ((null? expected-kws)
                           #true)
                          ((or (null? actual-kws) (keyword<? (car expected-kws) (car actual-kws)))
                           #false)
                          ((keyword<? (car actual-kws) (car expected-kws))
                           (loop expected-kws (cdr actual-kws)))
                          (else
                           (loop (cdr expected-kws) (cdr actual-kws))))))))))))


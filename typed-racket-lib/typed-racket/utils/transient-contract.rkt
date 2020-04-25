#lang racket/base

;; Extra contracts and tools for the Transient runtime

(provide
  procedure-arity-includes-keywords?
  transient-assert
  raise-transient-error)

(require
  (only-in racket/pretty pretty-format))

;; ---------------------------------------------------------------------------------------------------

;; procedure-arity-includes-keywords? : (-> procedure? (listof keyword?) (listof keyword?) boolean?)
;; Returns true if the procedure accepts calls that supply all mandatory keywords
;; and some optional keywords --- in the sense of racket/contract arity checking.
;; + function must declare all optional keywords as optional
;; + function may declare all mandatory keywords as either mandatory or optional
(define (procedure-arity-includes-keywords? f mand-kw* opt-kw*)
  (define-values [f-mand-kw* f-opt-kw*] (procedure-keywords f))
  ;; note: f-opt-kw* = (sort f-opt-kw* keyword<?)
  (define mand-ok/extra*
    ;; subtract f's mandatory keywords from the expected list (must be a prefix)
    (let loop ((expected-kw* mand-kw*)
               (actual-kw* f-mand-kw*))
      (cond
        [(null? actual-kw*)
         expected-kw*]
        [(null? expected-kw*)
         #f]
        [else
         (and (eq? (car expected-kw*) (car actual-kw*))
              (loop (cdr expected-kw*) (cdr actual-kw*)))])))
  (and mand-ok/extra*
       (let loop ((expected-kw* (sort (append mand-ok/extra* opt-kw*) keyword<?))
                  (actual-kw* f-opt-kw*))
         ;; match the remaining keywords against f's optionals
         (cond
          ((null? expected-kw*)
           #true)
          ((or (null? actual-kw*) (keyword<? (car expected-kw*) (car actual-kw*)))
           #false)
          ((eq? (car actual-kw*) (car expected-kw*))
           (loop (cdr expected-kw*) (cdr actual-kw*)))
          (else ;#(keyword<? actual expected)
           (loop expected-kw* (cdr actual-kw*)))))))

(define (transient-assert val pred ty-str ctx)
  (if (pred val)
    val
    (raise-transient-error val ty-str ctx)))

(define (raise-transient-error val ty ctx blame-entry*)
  (raise-arguments-error 'transient-assert
                         "value does not match static type"
                         "value" val
                         "type" (unquoted-printing-string ty)
                         "src" ctx
                         "blame" blame-entry*))

;;

(provide blame-map-ref blame-map-set!)

(define THE-BLAME-MAP (make-hasheq))

(struct blame-entry (
  type ;; expected
  ;; prev ;; eq-hash-code of "parent"
  dir ;; (or/c 'dom 'cod)
  ;; ctx ;; source-location-list?
) #:prefab)

(define (make-blame-entry ty prev-val dir ctx)
  (blame-entry ty #;(eq-hash-code prev-val) dir #;ctx))

(define (blame-map-ref v)
  (hash-keys (hash-ref THE-BLAME-MAP v (lambda () '#hash()))))

(define (blame-map-set! v ty prev dir ctx)
  (unless (eq? v (eq-hash-code v))
    (define be (make-blame-entry ty prev dir ctx))
    (hash-update! THE-BLAME-MAP v
                  (lambda (curr) (set-add curr be))
                  (lambda () (set-init be)))))

(define (set-init v)
  (hash v #true))

(define (set-add h v)
  (hash-set h v #true))

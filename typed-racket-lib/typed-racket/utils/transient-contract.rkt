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

(define (transient-assert val pred ty-str ctx from)
  (blame-map-set! val ty-str from)
  (if (pred val)
    val
    (begin
      (print-blame-map)
      (raise-transient-error val ty-str ctx (blame-map-ref val)))))

(define (raise-transient-error val ty ctx blame-entry*)
  (raise-arguments-error 'transient-assert
                         "value does not match static type"
                         "value" val
                         "type" (unquoted-printing-string ty)
                         "src" ctx
                         "blame" blame-entry*))

;; -----------------------------------------------------------------------------
;; --- blame map

(provide blame-map-ref blame-map-set!)

(define THE-BLAME-MAP (make-hasheq))

(define BLAME-DEBUG? #true)

(define blame-compress-key ;; (-> any/c any/c)
  (if BLAME-DEBUG?
    values
    eq-hash-code))

(define blame-source* '(
  cast
  require/typed
  dom cod
  car cdr
  list-elem list-rest
  mcar mcdr
  vector-elem
  box-elem
  hash-key hash-value
  sequence-elem sequence-rest
  stream-elem stream-rest
  ;; ... TBD, maybe should be identifiers?
))

(define blame-source-1* '(
  struct-elem
  object-field
  object-method-cod
))

;; Retic sources (mgd_transient.py)
;;   GETATTR = 0
;;   GETITEM = 1
;;   ARG = 2
;;   RETURN = 3
;; or this ... both in the same file
;;   GETATTR = 0 # include attr
;;   ARG = 1 #include position
;;   RET = 2
;;   GETITEM = 3

(define (blame-source? sym)
  (or (and (symbol? sym)
           (memq sym blame-source*))
      (and (pair? sym)
           (symbol? (car sym))
           #;(natural[struct] or symbol[object] (cdr sym)))))

(struct blame-entry (
  from ;; blame-source?
) #:prefab)

(struct cast-info blame-entry (
  type ;; string?, expected type
) #:prefab)

(struct check-info blame-entry (
  parent ;; eq-hash-code
) #:prefab)

;; make-blame-entry : (-> string? (or/c symbol? (cons/c any/c symbol?)) blame-entry?)
(define (make-blame-entry ty-str from)
  (if (pair? from)
    (check-info (cdr from) (eq-hash-code (car from)))
    (cast-info from ty-str)))

(define (blame-map-ref v)
  (hash-keys (hash-ref THE-BLAME-MAP (blame-compress-key v) (lambda () '#hash()))))

(define (blame-map-set! val ty-str from)
  (unless (eq? val (eq-hash-code val))
    (define be (make-blame-entry ty-str from))
    (hash-update! THE-BLAME-MAP (blame-compress-key val)
                  (lambda (curr) (set-add curr be))
                  (lambda () (set-init be)))))

(define (set-init v)
  (hash v #true))

(define (set-add h v)
  (hash-set h v #true))

(define (print-blame-map)
  (printf "BLAME MAP~n")
   (printf "ps ~a~n" (eq-hash-code apply))
  (for (((k v) (in-hash THE-BLAME-MAP)))
    (printf " (~s ~a(~n" k (if BLAME-DEBUG? (format "~a " (eq-hash-code k)) ""))
    (for ((vv (in-hash-keys v)))
      (printf "  ~s~n" vv))
    (printf " )~n"))
  (void))

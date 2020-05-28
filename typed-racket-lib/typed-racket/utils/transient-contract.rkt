#lang racket/base

;; Extra contracts and tools for the Transient runtime

(provide
  procedure-arity-includes-keywords?
  transient-assert
  raise-transient-error
  make-transient-provide-contract)

(require
  (only-in racket/contract blame-positive make-flat-contract)
  (only-in racket/pretty pretty-format)
  racket/lazy-require)

(lazy-require ;; avoid circular reference to type-contract.rkt
  (typed-racket/utils/transient-filter (value-type-match? sexp->type)))

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

(define (transient-assert val pred ty-datum ctx from)
  (blame-map-set! val ty-datum from)
  (if (pred val)
    val
    (begin
      (print-blame-map)
      (raise-transient-error val ty-datum ctx from))))

(define (raise-transient-error val ty-datum ctx from)
  (define boundary*
    (if (boundary? from)
      (list (make-blame-entry ty-datum from))
      (blame-map-boundary* val (cdr from) (blame-compress-key (car from)))))
  (void
    (let ((num-b (length boundary*)))
      (log-transient-error "blaming ~a boundar~a" num-b (if (= 1 num-b) "y" "ies")))
    (for ((b (in-list boundary*)))
      (log-transient-error "  ~s" b)))
  (raise-arguments-error 'transient-assert
                         "value does not match static type"
                         "value" val
                         "type" (sexp->type ty-datum)
                         "src" ctx))

(define (make-transient-provide-contract pred ty-datum ctx)
  (define ((lnp blame) val neg-party)
    (if (eq? neg-party 'incomplete-blame-from-provide.rkt)
      #true
      (let ((pos-party (blame-positive blame)))
        (blame-map-set! val ty-datum (list 'boundary 'provide ctx pos-party neg-party)))))
  (make-flat-contract
    #:name (format "transient-projection:~a" (object-name pred))
    #:late-neg-projection lnp))

;; -----------------------------------------------------------------------------
;; --- blame map

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

(define THE-BLAME-MAP (make-hasheq))

(define-logger transient)

(define blame-compress-key eq-hash-code)

(define blame-source* '(
  cast
  require/typed
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
  object-method-rng
  dom
  rng
))

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
  blame ;; ???
) #:prefab)

(struct check-info blame-entry (
  parent ;; eq-hash-code
) #:prefab)

(define (boundary? x)
  (and (pair? x) (eq? 'boundary (car x))))

;; make-blame-entry : (-> string? (or/c symbol? (cons/c any/c symbol?)) blame-entry?)
(define (make-blame-entry ty-datum from)
  (if (boundary? from)
    (cast-info (cadr from) ty-datum (cddr from))
    (check-info (cdr from) (eq-hash-code (car from)))))

(define (blame-map-ref v)
  (hash-keys (hash-ref THE-BLAME-MAP (blame-compress-key v) (lambda () '#hash()))))

(define (blame-map-set! val ty-datum from)
  (unless (eq? val (eq-hash-code val))
    (define be (make-blame-entry ty-datum from))
    (hash-update! THE-BLAME-MAP (blame-compress-key val)
                  (lambda (curr) (set-add curr be))
                  (lambda () (set-init be)))))

(define (set-init v)
  (hash v #true))

(define (set-add h v)
  (hash-set h v #true))

(define (print-blame-map)
  (log-transient-error "blame map")
  (for (((k v) (in-hash THE-BLAME-MAP)))
    (log-transient-error " (~s (" k)
    (for ((vv (in-hash-keys v)))
      (log-transient-error "  ~s" vv))
    (log-transient-error " )"))
  (void))

(define (blame-map-boundary* val init-action key)
  (let loop ([entry+path* (add-path* (blame-map-ref key) (list init-action))])
   (apply append
    (for/list ((e+p (in-list entry+path*)))
      (define e (car e+p))
      (define curr-path (cdr e+p))
      (cond
        [(check-info? e)
         (define parent (check-info-parent e))
         (define action (blame-entry-from e))
         (define new-path (cons action curr-path))
         (loop (add-path* (blame-map-ref parent) new-path))]
        [(cast-info? e)
         (define ty (cast-info-type e))
         (if (value-type-match? val ty curr-path (car (cast-info-blame e)))
           '()
           (list e))]
        [else
          (raise-argument-error 'blame-map-boundary* "blame-entry?" e)])))))

(define (add-path* entry* path)
  (for/list ((e (in-list entry*)))
    (cons e path)))

;(define (simple-blame-map-boundary* v)
;  (let loop ([entry* (blame-map-ref v)])
;   (apply append
;    (for/list ((e (in-list entry*)))
;      (cond
;        [(check-info? e)
;         (loop (blame-map-ref (check-info-parent e)))]
;        [(cast-info? e)
;         (list e)]
;        [else
;          (raise-argument-error 'blame-map-boundary* "blame-entry?" e)])))))

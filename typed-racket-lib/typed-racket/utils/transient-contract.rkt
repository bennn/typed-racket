#lang racket/base

;; Extra contracts and tools for the Transient runtime

(provide
  procedure-arity-includes-keywords?
  ;; 

  transient-assert
  ;; (-> value predicate ty-datum ctx from value)
  ;; If value matches predicate, update blame & return value.
  ;; Error otherwise

  arg-cast
  ;; (-> value from value)
  ;; Apply to function args, records a "cast"

  raise-transient-error
  make-transient-provide-contract)

(require
  (only-in racket/contract blame-positive make-flat-contract)
  (only-in racket/pretty pretty-format)
  racket/lazy-require
  typed-racket/utils/transient-contract-struct)

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
    (if (pre-boundary? from)
      (list (pre-boundary->boundary ty-datum from))
      (blame-map-boundary* val (cdr from) (blame-compress-key (car from)))))
  (void
    (let ((num-b (length boundary*)))
      (log-transient-info "blaming ~a boundar~a" num-b (if (= 1 num-b) "y" "ies")))
    (for ((b (in-list boundary*)))
      (log-transient-info "  ~s" (list (boundary-pos b) (boundary-neg b)))))
  (raise
    (exn:fail:contract:blame:transient
      (format
        "transient-assert: value does not match static type\n  value: ~s\n  type: ~s\n  src: ~s"
        val ty-datum #;(sexp->type ty-datum) ctx)
      (current-continuation-marks)
      boundary*)))

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

(define THE-BLAME-MAP (make-hasheq))

(define blame-compress-key eq-hash-code)

(define (pre-boundary? x)
  (and (pair? x) (eq? 'boundary (car x))))

;; make-blame-entry : (-> any/c (or/c symbol? (cons/c any/c symbol?)) blame-entry?)
(define (make-blame-entry ty-datum from)
  (if (pre-boundary? from)
    (pre-boundary->cast-info ty-datum from)
    (check-info (cdr from) (eq-hash-code (car from)))))

(define (cast-info->boundary ci)
  (define ty (cast-info-type ci))
  (define blame-val (cast-info-blame ci))
  (define pos-mod
    (let ((m (cadr blame-val)))
      (if (or (path? m)
              (and (pair? m) (path? (car m)))) ;; submod path
        m
        (let ((mpi (variable-reference->module-path-index (car blame-val))))
          (resolved-module-path-name (module-path-index-resolve (module-path-index-join m mpi)))))))
  (make-boundary pos-mod (caddr blame-val) ty))

(define (pre-boundary->cast-info ty-datum from)
  (cast-info (cadr from) ty-datum (cddr from)))

(define (pre-boundary->boundary ty-datum from)
  (cast-info->boundary (pre-boundary->cast-info ty-datum from)))

(define (blame-map-ref v)
  (define entry# (hash-ref THE-BLAME-MAP (blame-compress-key v) (lambda () '#hash())))
  (map car (sort (hash->list entry#) > #:key cdr)))

(define (blame-map-set! val ty-datum from)
  (unless (eq? val (eq-hash-code val))
    (define be (make-blame-entry ty-datum from))
    (hash-update! THE-BLAME-MAP (blame-compress-key val)
                  (lambda (curr) (blame-entry*-add curr be))
                  (lambda () (blame-entry*-init be)))))

(define (arg-cast val from)
  (blame-map-set! val #f from)
  val)

(define (blame-entry*-init v)
  (hash v 0))

(define (blame-entry*-add h v)
  (if (hash-has-key? h v)
    h
    (hash-set h v (hash-count h))))

(define (print-blame-map)
  (log-transient-info "blame map")
  (for (((k v) (in-hash THE-BLAME-MAP)))
    (log-transient-info " (~s (" k)
    (for ((vv (in-hash-keys v)))
      (log-transient-info "  ~s" vv))
    (log-transient-info " )"))
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
         (define new-path
           (if (noop-action? action)
             curr-path
             (cons action curr-path)))
         (loop (add-path* (blame-map-ref parent) new-path))]
        [(cast-info? e)
         (define ty (cast-info-type e))
         (define blame-val (cast-info-blame e))
         (if (with-handlers ((exn:fail? (lambda (ex)
                                               (printf "transient: internal error during value/type match~n value ~s~n type ~s~n message ~s~n" val ty (exn-message ex))
                                               #f)))
               (value-type-match? val ty curr-path (variable-reference->module-path-index (car blame-val))))
           '()
           (list (cast-info->boundary e)))]
        [else
          (raise-argument-error 'blame-map-boundary* "blame-entry?" e)])))))

(define (add-path* entry* path)
  (for/list ((e (in-list entry*)))
    (cons e path)))


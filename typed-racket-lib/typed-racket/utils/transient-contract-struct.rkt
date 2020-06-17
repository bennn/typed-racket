#lang racket/base

;; Data definitions for Transient runtime errors and blame

(provide
  (struct-out exn:fail:contract:blame:transient)
  ;; transient runtime error
  ;; sub-struct of exn:fail:contract:blame
  ;; the `exn:fail:contract:blame-object` field contains a list of `boundary?`

  transient-get-blame
  ;; alias for `exn:fail:contract:blame-object`

  (struct-out boundary)
  make-boundary

  (struct-out blame-entry)
  (struct-out cast-info)
  (struct-out check-info)

  blame-source?
  ;; (-> any/c boolean?)

  transient-logger
  log-transient-info
  log-transient-debug
  log-transient-warning
  log-transient-error
  log-transient-fatal)

(require
  (only-in racket/contract exn:fail:contract:blame exn:fail:contract:blame-object))

(define-logger transient)

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


(struct exn:fail:contract:blame:transient exn:fail:contract:blame ())

(define transient-get-blame exn:fail:contract:blame-object)

;; pos  = server : path-string?
;; neg  = client : path-string?
;; type = original type at boundary : any/c (S-expression)
(struct boundary (pos neg type) #:prefab)
(define make-boundary boundary)


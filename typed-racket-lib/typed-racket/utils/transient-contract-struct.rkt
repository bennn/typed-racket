#lang racket/base

(provide
  (struct-out exn:fail:contract:blame:transient))

(require
  (only-in racket/contract exn:fail:contract:blame))

(struct exn:fail:contract:blame:transient exn:fail:contract:blame ())


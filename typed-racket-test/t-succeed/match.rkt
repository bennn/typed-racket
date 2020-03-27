#lang typed-scheme #:transient

(require scheme/match)

(match 1
  [2 3]
  [x 4])

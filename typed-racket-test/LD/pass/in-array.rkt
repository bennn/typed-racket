#lang typed/racket #:locally-defensive

;; Use 'in-array'
;; inspired by jpeg benchmark


(require math/array)

(: read-dct-scan (-> (Array Integer) Void))
(define (read-dct-scan dest)
  (for ((mcu (in-array dest)))
    (void)))

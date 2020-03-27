#lang typed-scheme #:transient

(let: ([x : Number 1])
      (let-syntax ([m (syntax-rules ()
			[(_) x])])
	(m)))

#lang typed-scheme #:transient
(require/typed
 scheme/base
 [opaque WeakBox weak-box?]
 [make-weak-box (Any -> WeakBox)])

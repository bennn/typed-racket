#lang typed/racket  #:transient

(ann (regexp-match* "foo" "foobar") (Listof String))
(ann (regexp-match* "foo" #"foobar") (Listof Bytes))
(ann (regexp-match* #"foo" "foobar") (Listof Bytes))
(ann (regexp-match* #"foo" #"foobar") (Listof Bytes))

#lang info
(define collection "readtable-extra")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/readtable-extra.scrbl" ())))
(define pkg-desc "A make-readtable drop-in replacement with an ability to yield to other tables")
(define version "0.0")
(define pkg-authors '(sorawee))

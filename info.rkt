#lang info
(define collection "mathle")
(define deps '("base"
               "curly-fn-lib"
               "try-catch-finally-lib"
               ; parser
               "megaparsack-lib"
               "functional-lib"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(linzizhuan))
(define license '(Apache-2.0 OR MIT))
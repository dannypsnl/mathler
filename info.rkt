#lang info
(define collection "mathler")
(define deps
  '("base" "curly-fn-lib"
           "try-catch-finally-lib"
           "readline-lib"
           ; parser
           "megaparsack-lib"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(linzizhuan))
(define license '(Apache-2.0 OR MIT))

(define primary-file "main.rkt")
(define racket-launcher-names (list "mathler"))
(define racket-launcher-libraries (list "main.rkt"))

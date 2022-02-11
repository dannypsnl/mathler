#lang racket/base
(provide pretty
         red)

(require syntax/parse/define
         (for-syntax racket/base))

(define (pretty r)
  (apply string-append
         (map (lambda (p)
                (case (cdr p)
                  [(green) (green (car p))]
                  [(yellow) (yellow (car p))]
                  [(gray) (black (car p))]))
              r)))

(define (color c s)
  (format "\e[1;~am~a\e[0m" c s))
(define-syntax-parser defcolor
  [(_ name:id c:integer)
   #'(define (name s)
       (color c s))])

(defcolor red 31)
(defcolor black 40)
(defcolor green 42)
(defcolor yellow 43)

(module+ main
  (require "puzzle.rkt")
  (define test-target (puzzle "60/5*9" 108))
  (displayln (pretty (answer test-target "27+9*9"))))

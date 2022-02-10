#lang racket/base
(require syntax/parse/define
         (for-syntax racket/base))

(define (color c s)
  (format "\e[1;~am~a\e[0m" c s))
(define-syntax-parser defcolor
  [(_ name:id c:integer)
   (with-syntax ([bg:name (datum->syntax #'name (string->symbol (string-append "bg:" (symbol->string (syntax->datum #'name)))))])
     #'(define (name s) (color c s)))])

(defcolor black 30)
(defcolor green 32)
(defcolor yellow 33)

(displayln (green "hello, world!"))
(displayln (yellow "hello, world!"))
(displayln (black "hello, world!"))

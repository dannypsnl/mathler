#lang racket/base
(provide black green yellow)

(require syntax/parse/define
         (for-syntax racket/base))

(define (color c s)
  (format "\e[1;~am~a\e[0m" c s))
(define-syntax-parser defcolor
  [(_ name:id c:integer)
   (with-syntax ([bg:name (datum->syntax #'name (string->symbol (string-append "bg:" (symbol->string (syntax->datum #'name)))))])
     #'(define (name s) (color c s)))])

(defcolor black 40)
(defcolor green 42)
(defcolor yellow 43)

#lang racket/base
(require syntax/parse/define
         (for-syntax racket/base))

(define (color c s)
  (format "\e[~am~a\e[0m" c s))
(define-syntax-parser defcolor
  [(_ name:id c:integer)
   (with-syntax ([bg:name (datum->syntax #'name (string->symbol (string-append "bg:" (symbol->string (syntax->datum #'name)))))])
     #'(begin
         (define (name s) (color c s))
         (define (bg:name s) (color (+ 10 c) s))))])

(defcolor black 30)
(defcolor red 31)
(defcolor green 32)
(defcolor brown 33)
(defcolor blue 34)
(defcolor purple 35)
(defcolor cyan 36)
(defcolor light-gray 37)

(displayln (black "hello, world!"))
(displayln (red "hello, world!"))
(displayln (green "hello, world!"))
(displayln (brown "hello, world!"))
(displayln (blue "hello, world!"))
(displayln (purple "hello, world!"))
(displayln (cyan "hello, world!"))
(displayln (light-gray "hello, world!"))
(displayln (bg:black "hello, world!"))
(displayln (bg:red "hello, world!"))
(displayln (bg:green "hello, world!"))
(displayln (bg:brown "hello, world!"))
(displayln (bg:blue "hello, world!"))
(displayln (bg:purple "hello, world!"))
(displayln (bg:cyan "hello, world!"))
(displayln (bg:light-gray "hello, world!"))

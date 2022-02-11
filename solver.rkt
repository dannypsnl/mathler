#lang racket
(require racket/random
         "puzzle.rkt")

(define test-target (puzzle "60/5*9" 108))

(define (generate-solution result)
  (define s (string->list "123456789"))
  (define s2 (string->list "01234+-*/56789"))
  (define s3 (string->list "0123456789"))
  (define p1 (random-ref s))
  (define p2 (random-ref s2))
  (define p3 (if (member p2 (string->list "+-*/")) (random-ref s) (random-ref s2)))
  (define p4 (if (member p3 (string->list "+-*/")) (random-ref s) (random-ref s2)))
  (define p5 (if (member p4 (string->list "+-*/")) (random-ref s) (random-ref s2)))
  (define p6 (if (eq? #\/ p5) (random-ref s) (random-ref s3)))
  (define solution (string p1 p2 p3 p4 p5 p6))
  (if (= result (calculate solution))
      solution
      (generate-solution result)))

(answer test-target (generate-solution 108))
(answer test-target (generate-solution 108))
(answer test-target (generate-solution 108))

#lang racket
(require racket/random
         "puzzle.rkt"
         "color.rkt")

(define test-target (puzzle "60/5*9" 108))

(define s (list->mutable-set (string->list "123456789")))
(define s2 (list->mutable-set (string->list "01234+-*/56789")))
(define s3 (list->mutable-set (string->list "0123456789")))

(define (generate-solution result)
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

(define (update-set response)
  (for ([p response])
    (match-define (cons c status) p)
    (case status
      [(gray)
       (set-remove! s c)
       (set-remove! s2 c)
       (set-remove! s3 c)
       ]
      [else (void)])))

(let loop ()
  (define r (answer test-target (generate-solution 108)))
  (displayln (pretty r))
  (update-set r)
  (if (solved? r)
      (void)
      (loop)))

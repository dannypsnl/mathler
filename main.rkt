#lang racket/base
(require racket/match
         racket/string
         racket/random
         try-catch-finally
         "puzzle.rkt")

(define (gameloop)
  (define p (generate-puzzle))
  (let loop ()
    (printf "~a > " (puzzle-compute-answer p))
    (define solution-string (read-line))
    (cond
      [(eof-object? solution-string)
         (displayln "  ")]
      [else
       (try
        (define r (answer p solution-string))
        (displayln r)
        (cond
          [(solved? r) (displayln "solved")]
          [else (loop)])
        (catch (e)
          (displayln e)
          (loop)))])))

(module+ main
  (gameloop))

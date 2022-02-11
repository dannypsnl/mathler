#lang racket/base
(require try-catch-finally
         "puzzle.rkt"
         "solver.rkt"
         "color.rkt")

(define (gameloop)
  (define p (generate-puzzle))
  (let loop ()
    (printf "(~a)> " (puzzle-compute-answer p))
    (define input (read-line))
    (cond
      [(eof-object? input) (displayln "  ")]
      [(equal? "auto" input) (solve p)]
      [else
       (try (define r (answer p input))
            (displayln (pretty r))
            (cond
              [(solved? r) (displayln "solved")]
              [else (loop)])
            (catch (exn:fail? e) (displayln (red (exn-message e))) (loop)))])))

(module+ main
  (gameloop))

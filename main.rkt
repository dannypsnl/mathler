#lang racket/base
(require try-catch-finally
         readline/readline
         "puzzle.rkt"
         "solver.rkt"
         "color.rkt")

(define (gameloop)
  (define p (generate-puzzle))
  (let loop ()
    (define input (readline (format "(~a)> " (puzzle-compute-answer p))))
    (cond
      [(eof-object? input) (displayln "  ")]
      [(equal? "auto" input) (solve p)]
      [else (add-history input)
            (try (define response (answer p input))
                 (displayln (pretty response))
                 (cond
                   [(solved? response) (displayln "solved")]
                   [else (loop)])
                 (catch (exn:fail? e) (displayln (red (exn-message e))) (loop)))])))

(module+ main
  (gameloop))

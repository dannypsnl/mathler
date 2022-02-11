#lang racket/base
(require try-catch-finally
         "puzzle.rkt"
         "color.rkt")

(define (gameloop)
  (define p (generate-puzzle))
  (let loop ()
    (printf "~a > " (puzzle-compute-answer p))
    (define solution-string (read-line))
    (cond
      [(eof-object? solution-string) (displayln "  ")]
      [else
       (try (define r (answer p solution-string))
            (displayln (pretty r))
            (cond
              [(solved? r) (displayln "solved")]
              [else (loop)])
            (catch (exn:fail? e) (displayln (red (exn-message e))) (loop)))])))

(module+ main
  (gameloop))

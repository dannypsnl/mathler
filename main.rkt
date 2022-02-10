#lang curly-fn racket/base
(require try-catch-finally
         "puzzle.rkt"
         "color.rkt")

(define (pretty r)
  (apply string-append
         (map #{(case (cdr %)
                  [(green) (green (car %))]
                  [(yellow) (yellow (car %))]
                  [(gray) (black (car %))])}
              r)))

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
        (displayln (pretty r))
        (cond
          [(solved? r) (displayln "solved")]
          [else (loop)])
        (catch (e)
               (displayln e)
               (loop)))])))

(module+ main
  (gameloop))

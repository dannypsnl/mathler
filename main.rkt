#lang racket/base
(require racket/match
         try-catch-finally
         readline/readline
         "puzzle.rkt"
         "solver.rkt"
         "color.rkt"
         "response-parser.rkt")

(define (gameloop [ans #f])
  (define p (if ans (generate-puzzle ans) (generate-puzzle)))
  (let loop ()
    (define input (readline (format "(~a)> " (puzzle-compute-answer p))))
    (cond
      [(eof-object? input) (displayln "")]
      [(equal? "auto" input) (solve p)]
      [else
       (add-history input)
       (try (define response (answer p input))
            (displayln (pretty response))
            (cond
              [(solved? response) (displayln "solved")]
              [else (loop)])
            (catch (exn:fail? e) (displayln (red (exn-message e))) (loop)))])))

(define (solveloop target)
  (define-values (generate-solution learn) (generate-solver))
  (let loop ()
    (define solution (generate-solution target))
    (displayln solution)
    (define input (readline "> "))
    (cond
      [(eof-object? input) (displayln "")]
      [else
       (define response (map cons (string->list solution) (parse-response input)))
       (displayln (pretty response))
       (learn response)
       (if (solved? response) (void) (loop))])))

(module+ main
  (require racket/cmdline)

  (command-line #:program "mathler"
                #:args args
                (match args
                  ['("play") (gameloop)]
                  [(list "play" answer) (gameloop answer)]
                  [(list "solve" target) (solveloop (string->number target))]
                  [_ (usage)])))

(define (usage)
  (display
   "usage: mathler <command> [<args>]

Commands:

  play <optional-puzzle>
     Start a new game
  solve <target>
     Interactive solving a mathler puzzle with the given target
"))

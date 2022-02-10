#lang curly-fn racket/base
(require racket/match
         racket/string
         racket/random
         try-catch-finally
         "parser.rkt")

(struct puzzle (answer compute-answer)
  #:transparent)

(define test-target (puzzle "60/5*9" 108))

(define (calculate str)
  (define e (parse-expr str))
  (define (inner e)
    (match e
      [`(+ ,a ,b) (+ (inner a) (inner b))]
      [`(- ,a ,b) (- (inner a) (inner b))]
      [`(* ,a ,b) (* (inner a) (inner b))]
      [`(/ ,a ,b) (/ (inner a) (inner b))]
      [_ e]))
  (inner e))

(define (answer p solution-string)
  (match-define (puzzle answer compute-answer) p)
  (define solution-result (calculate solution-string))
  (unless (= 6 (string-length solution-string))
    (error 'solution "invalid length"))
  (unless (= solution-result compute-answer)
    (error 'solution "invalid: ~a, solution-result: ~a"
           solution-string
           solution-result))
  (for/list ([c solution-string]
             [ac answer])
    (cond
      [(eq? c ac) (cons c 'green)]
      [(string-contains? answer (string c)) (cons c 'yellow)]
      [else (cons c 'gray)])))

(define (solved? l)
  (andmap #{eq? 'green}
          (map cdr l)))

(solved? (answer test-target "27+9*9"))

(define (generate-puzzle)
  (define s (string->list "123456789"))
  (define s2 (string->list "01234+-*/56789"))
  (define s3 (string->list "0123456789"))
  ; FIXME: invalid string like 1*+20/6 should be rejected
  (define answer (string (random-ref s) (random-ref s2) (random-ref s2) (random-ref s2) (random-ref s2) (random-ref s3)))
  (if (and (integer? (calculate answer))
           (< (calculate answer) 1000))
      (puzzle answer
              (calculate answer))
      (generate-puzzle)))

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

(gameloop)

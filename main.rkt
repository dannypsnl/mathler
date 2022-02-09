#lang curly-fn racket/base
(require racket/match
         racket/string
         "parser.rkt")

(struct puzzle (answer compute-answer)
  #:transparent)

(define test-target (puzzle "60/5*9" 108))

(define (calculate e)
  (match e
    [`(+ ,a ,b) (+ (calculate a) (calculate b))]
    [`(- ,a ,b) (- (calculate a) (calculate b))]
    [`(* ,a ,b) (* (calculate a) (calculate b))]
    [`(/ ,a ,b) (/ (calculate a) (calculate b))]
    [_ e]))

(define (answer p solution-string)
  (match-define (puzzle answer compute-answer) p)
  (define solution-result (calculate (parse-expr solution-string)))
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

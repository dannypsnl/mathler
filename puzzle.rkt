#lang curly-fn racket/base
(provide generate-puzzle
         answer
         solved?
         (struct-out puzzle)
         calculate)

(require racket/random
         racket/match
         racket/set
         racket/string
         "parser.rkt")

(struct puzzle (answer compute-answer) #:transparent)

(define (gen-answer)
  (define s (string->list "123456789"))
  (define s2 (string->list "01234+-*/56789"))
  (define s3 (string->list "0123456789"))
  (define p1 (random-ref s))
  (define p2 (random-ref s2))
  (define p3 (if (member p2 (string->list "+-*/")) (random-ref s) (random-ref s2)))
  (define p4 (if (member p3 (string->list "+-*/")) (random-ref s) (random-ref s2)))
  (define p5 (if (member p4 (string->list "+-*/")) (random-ref s) (random-ref s2)))
  (define p6 (if (eq? #\/ p5) (random-ref s) (random-ref s3)))
  (string p1 p2 p3 p4 p5 p6))

(define (generate-puzzle [answer (gen-answer)])
  (if (and (integer? (calculate answer)) (< 0 (calculate answer) 200))
      (puzzle answer (calculate answer))
      (generate-puzzle)))

(define (solved? l)
  (andmap #{eq? 'green} (map cdr l)))

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
    (error 'invalid "expected length is 6"))
  (unless (= solution-result compute-answer)
    (error 'invalid
           "~a = ~a, expected: ~a"
           solution-string
           (calculate solution-string)
           compute-answer))
  (define ans-list (string->list answer))
  (for/list ([c solution-string] [ac answer] [i (string-length solution-string)])
    (cond
      [(eq? c ac)
       (set! ans-list (remove c ans-list))
       (cons c 'green)]
      [(member c ans-list) (cons c 'yellow)]
      [else (cons c 'gray)])))

(module+ test
  (require rackunit)

  (check-false (solved? (answer (generate-puzzle "60/5*9") "27+9*9")))
  (check-equal?
   (answer (generate-puzzle "49/7+5") "35/7+7")
   '((#\3 . gray) (#\5 . yellow) (#\/ . green) (#\7 . green) (#\+ . green) (#\7 . gray))))

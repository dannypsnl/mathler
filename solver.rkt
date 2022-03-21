#lang racket/base
(provide solve
         generate-solver)

(require racket/string
         racket/set
         racket/match
         "puzzle.rkt"
         "color.rkt")

(define (solve p)
  (define-values (generate-solution learn) (generate-solver))
  (let loop ()
    (define r (answer p (generate-solution (puzzle-compute-answer p))))
    (displayln (pretty r))
    (learn r)
    (if (solved? r) (void) (loop))))

(define (remove-impossible-solution s previous-previous-p previous-p)
  (cond
    [(string-contains? "+-*" (string previous-p))
     (set-subtract (list->set (set->list s)) (set #\+ #\- #\* #\/))]
    [(eq? previous-p #\/) (set-subtract (list->set (set->list s)) (set #\+ #\- #\* #\/ #\0))]
    [(and (string-contains? "+-*/" (string previous-previous-p)) (eq? previous-p #\0))
     (set #\+ #\- #\* #\/)]
    [else s]))

(define (yellow-in-solution? s yellow*)
  (for/and ([c yellow*])
    (string-contains? s (string c))))

(define (ensure-left-one-remove! set e)
  (if (= 1 (set-count set)) (void) (set-remove! set e)))

(define (generate-solver)
  (define s1 (list->mutable-set (string->list "123456789")))
  (define s2 (list->mutable-set (string->list "01234+-*/56789")))
  (define s3 (list->mutable-set (string->list "01234+-*/56789")))
  (define s4 (list->mutable-set (string->list "01234+-*/56789")))
  (define s5 (list->mutable-set (string->list "01234+-*/56789")))
  (define s6 (list->mutable-set (string->list "0123456789")))
  (define yellow* (mutable-set))

  (define (generate-solution result)
    (let/cc return
      (for ([p1 (in-set s1)])
        (for ([p2 (in-set s2)])
          (for ([p3 (in-set (remove-impossible-solution s3 p1 p2))])
            (for ([p4 (in-set (remove-impossible-solution s4 p2 p3))])
              (for ([p5 (in-set (cond
                                  [(string-contains? "+-*/" (string p4))
                                   (set-subtract (list->set (set->list s5)) (set #\+ #\- #\* #\/ #\0))]
                                  [(and (string-contains? "+-*/" (string p3)) (eq? p4 #\0))
                                   (set #\+ #\- #\* #\/)]
                                  [else s5]))])
                (for ([p6 (in-set (if (eq? p5 #\/) (set-remove (list->set (set->list s6)) #\0) s6))])
                  (define solution (string p1 p2 p3 p4 p5 p6))
                  (when (and (= result (calculate solution)) (yellow-in-solution? solution yellow*))
                    (return solution))))))))
      (error 'unsolvable "No solution found")))

  (define (learn response)
    (for ([p response] [i (in-range 0 6)])
      (match-define (cons c status) p)
      (case status
        [(gray)
         (if (set-member? yellow* c)
             (case i
               [(0) (set-remove! s1 c)]
               [(1) (set-remove! s2 c)]
               [(2) (set-remove! s3 c)]
               [(3) (set-remove! s4 c)]
               [(4) (set-remove! s5 c)]
               [(5) (set-remove! s6 c)])
             (begin
               (ensure-left-one-remove! s1 c)
               (ensure-left-one-remove! s2 c)
               (ensure-left-one-remove! s3 c)
               (ensure-left-one-remove! s4 c)
               (ensure-left-one-remove! s5 c)
               (ensure-left-one-remove! s6 c)))]
        [(green)
         (case i
           [(0) (set! s1 (mutable-set c))]
           [(1) (set! s2 (mutable-set c))]
           [(2) (set! s3 (mutable-set c))]
           [(3) (set! s4 (mutable-set c))]
           [(4) (set! s5 (mutable-set c))]
           [(5) (set! s6 (mutable-set c))])]
        [(yellow)
         (set-add! yellow* c)
         (case i
           [(0) (set-remove! s1 c)]
           [(1) (set-remove! s2 c)]
           [(2) (set-remove! s3 c)]
           [(3) (set-remove! s4 c)]
           [(4) (set-remove! s5 c)]
           [(5) (set-remove! s6 c)])])))

  (values generate-solution learn))

(module+ main
  (define (show p)
    (printf "solving: ~a~n" (puzzle-answer p))
    (solve p))

  (show (generate-puzzle))
  (show (generate-puzzle "5*31-4"))
  (show (generate-puzzle "49/7+5"))
  (show (generate-puzzle "28/7*6"))
  (show (generate-puzzle "86/2*3")))

#lang racket
(provide solve
         generate-solver)

(require racket/random
         try-catch-finally
         "puzzle.rkt"
         "color.rkt")

(define (solve p)
  (define-values (generate-solution learn) (generate-solver))
  (let loop ()
    (define r (answer p (generate-solution (puzzle-compute-answer p))))
    (displayln (pretty r))
    (learn r)
    (if (solved? r) (void) (loop))))

(define (handle-tmp-set s previous-previous-p previous-p)
  (define new-s
    (cond
      [(string-contains? "+-*" (string previous-p))
       (set-subtract (list->set (set->list s)) (set #\+ #\- #\* #\/))]
      [(eq? previous-p #\/) (set-subtract (list->set (set->list s)) (set #\+ #\- #\* #\/ #\0))]
      [(and (string-contains? "+-*/" (string previous-previous-p)) (eq? previous-p #\0))
       (set #\+ #\- #\* #\/)]
      [else s]))
  (if (set-empty? new-s) (random-ref s) (random-ref new-s)))

(define (yellow-in? s yellow*)
  (for/and ([c yellow*])
    (string-contains? s (string c))))

(define (set-last-one-remove! set e)
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
    (define p1 (random-ref s1))
    (define p2 (random-ref s2))
    (define p3 (handle-tmp-set s3 p1 p2))
    (define p4 (handle-tmp-set s4 p2 p3))
    (define p5 (handle-tmp-set s5 p3 p4))
    (define p6
      (cond
        [(eq? p5 #\/) (random-ref (set-remove (list->set (set->list s6)) #\0))]
        [(and (string-contains? "+-*/" (string p4)) (eq? p5 #\0)) (set #\+ #\- #\* #\/)]
        [else (random-ref s6)]))
    (define solution (string p1 p2 p3 p4 p5 p6))
    (try (if (and (= result (calculate solution)) (yellow-in? solution yellow*))
             solution
             (generate-solution result))
         (catch (e) (generate-solution result))))

  (define (learn response)
    (for ([p response] [i (in-range 0 6)])
      (match-define (cons c status) p)
      (case status
        [(gray)
         (set-last-one-remove! s1 c)
         (set-last-one-remove! s2 c)
         (set-last-one-remove! s3 c)
         (set-last-one-remove! s4 c)
         (set-last-one-remove! s5 c)
         (set-last-one-remove! s6 c)]
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
  (define test-target (generate-puzzle))
  (solve test-target))

(module+ test
  ; in this case, if we guess 35/7+7, it should report '(b y g g g b)
  (define p (generate-puzzle "49/7+5"))

  (solve p))

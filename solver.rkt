#lang racket
(require racket/random
         try-catch-finally
         "puzzle.rkt"
         "color.rkt")

(define s1 (list->mutable-set (string->list "123456789")))
(define s2 (list->mutable-set (string->list "01234+-*/56789")))
(define s3 (list->mutable-set (string->list "01234+-*/56789")))
(define s4 (list->mutable-set (string->list "01234+-*/56789")))
(define s5 (list->mutable-set (string->list "01234+-*/56789")))
(define s6 (list->mutable-set (string->list "0123456789")))

(define (handle-tmp-set s pp)
  (define new-s (cond
                  [(member pp (string->list "+-*"))
                   (set-subtract (list->set (set->list s)) (set #\+ #\- #\* #\/))]
                  [(eq? pp #\/)
                   (set-subtract (list->set (set->list s)) (set #\+ #\- #\* #\/ #\0))]
                  [else s]))
  (if (set-empty? new-s)
      (random-ref s)
      (random-ref new-s)))

(define (generate-solution result)
  (define p1 (random-ref s1))
  (define p2 (random-ref s2))
  (define p3 (handle-tmp-set s3 p2))
  (define p4 (handle-tmp-set s4 p3))
  (define p5 (handle-tmp-set s5 p4))
  (define p6 (if (eq? p5 #\/)
                 (random-ref (set-remove (list->set (set->list s6)) #\0))
                 (random-ref s6)))
  (define solution (string p1 p2 p3 p4 p5 p6))
  (try
   (if (= result (calculate solution))
       solution
       (generate-solution result))
   (catch (e)
          (generate-solution result))))

(define (update-set response)
  (for ([p response]
        [i (in-range 0 5)])
    (match-define (cons c status) p)
    (case status
      [(gray)
       (set-remove! s1 c)
       (set-remove! s2 c)
       (set-remove! s3 c)
       (set-remove! s4 c)
       (set-remove! s5 c)
       (set-remove! s6 c)]
      [(green) (case i
                 [(0) (set-intersect! s1 (set c))]
                 [(1) (set-intersect! s2 (set c))]
                 [(2) (set-intersect! s3 (set c))]
                 [(3) (set-intersect! s4 (set c))]
                 [(4) (set-intersect! s5 (set c))]
                 [(5) (set-intersect! s6 (set c))])]
      [(yellow) (case i
                  [(0) (set-remove! s1 c)]
                  [(1) (set-remove! s2 c)]
                  [(2) (set-remove! s3 c)]
                  [(3) (set-remove! s4 c)]
                  [(4) (set-remove! s5 c)]
                  [(5) (set-remove! s6 c)])])))

(module+ main
  (define test-target (generate-puzzle))

  (let loop ()
    (define r (answer test-target (generate-solution (puzzle-compute-answer test-target))))
    (displayln (pretty r))
    (update-set r)
    (if (solved? r)
        (void)
        (loop))))

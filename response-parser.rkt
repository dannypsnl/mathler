#lang racket/base
(provide parse-response)

(require megaparsack
         megaparsack/text
         data/applicative
         data/monad)

(define green/p (do (char/p #\g) (pure 'green)))
(define yellow/p (do (char/p #\y) (pure 'yellow)))
(define gray/p (do (char/p #\b) (pure 'gray)))

(define response/p
  (do [e <- (many/p (or/p green/p yellow/p gray/p) #:sep (many/p space/p) #:min 6 #:max 6)] (pure e)))

(define (parse-response s)
  (parse-result! (parse-string response/p s)))

(module+ test
  (require rackunit)

  (check-equal? (parse-response "ggyybb") '(green green yellow yellow gray gray))
  (check-equal? (parse-response "g g  y y b   b") '(green green yellow yellow gray gray)))

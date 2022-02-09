#lang racket/base
(provide parse-expr)

(require racket/match
         megaparsack
         megaparsack/text
         data/applicative
         data/monad)

(define (parse-expr s)
  (parse-result! (parse-string expr/p s)))

(define (c->s c)
  (case c
    [(#\+) '+]
    [(#\-) '-]
    [(#\*) '*]
    [(#\/) '/]
    [else c]))

(define (op/p op-list)
  (do [op <- (or/p (one-of/p op-list
                             (lambda (a b)
                               (eq? a (c->s b))))
                   void/p)]
    (pure (c->s op))))
(define factor/p
  (do [expr <- integer/p]
    (pure expr)))
(define (binary/p high-level/p op-list)
  (do [e <- high-level/p]
    ; `es` parse operator then high-level unit, for example, `* 1`.
    ; therefore, this parser would stop when the operator is not expected(aka. operator is in op-list)
    ; rely on this fact we can leave this loop
    [es <- (many/p (do [op <- (op/p op-list)]
                     [e <- high-level/p]
                     (pure (list op e))))]
    (pure (foldl
           (λ (op+rhs lhs)
             (match op+rhs
               [(list op rhs)
                (list op lhs rhs)]))
           e es))))
(define mul:div/p
  (binary/p factor/p '(* /)))
(define add:sub/p
  (binary/p mul:div/p '(+ -)))
(define expr/p add:sub/p)

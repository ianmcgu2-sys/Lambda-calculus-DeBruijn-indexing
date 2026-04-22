#lang racket
(require "lambda-interp.rkt")

(define (subst bdy exp index)
  (match bdy
    [(DBvar n)
     (DBvar n)]
    [(DBapp fst snd)
     (DBapp (subst fst exp index)
            (subst snd exp index))]
    [(DBabs expr)
     (DBabs (subst expr exp (add1 index)))]
    [(? number? n) (cond [(= index n) exp] [else n])]
))

(define (debruijn-interp-head ast)
  (match ast
    [(DBapp (DBabs bdy) exp)
     (debruijn-interp-head (subst bdy exp 1))]
    [(DBapp fst snd) (local
    [(define interp-fst (debruijn-interp-head fst))]
     (if (DBabs? interp-fst)
         (debruijn-interp-head (DBapp interp-fst snd))
         (DBapp interp-fst snd)))]
    [x x]))

(define (debruijn-interp-inner ast)
  (match ast
    [(DBapp fst snd)
     (DBapp (debruijn-interp fst)
            (debruijn-interp snd))]
    [(DBabs body)
     (DBabs (debruijn-interp body))]
    [x x]))

(define (debruijn-interp ast)
  (debruijn-interp-inner (debruijn-interp-head ast)))

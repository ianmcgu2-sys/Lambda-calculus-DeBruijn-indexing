#lang racket

(require "lambda-interp.rkt")

;;DBvar is for free variables only, bound variables are represented with natural numbers
(define-struct DBvar (id))
(define-struct DBapp (fst snd))
(define-struct DBabs (bdy))

;  expr = Nat
;       | (make-DBvar id)
;       | (make-DBapp expr expr)
;       | (make-DBabs expr)

(define (search var exp)
  (search-help var exp 1))
(define (search-help var exp index)
  (cond
    [(empty? exp) false]
    [(symbol=? var (first exp)) index]
    [else (search-help var (rest exp) (add1 index))]))

(define (DB exp acc)
  (match exp
      [(Var id) (if (search id acc) (search id acc) (make-DBvar id))]
      [(App fst snd) (make-DBapp (DB fst acc) (DB snd acc))]
      [(Abs par bdy) (make-DBabs (DB bdy (cons par acc)))]))
    

(define (lambda->debruijn exp)
  (DB exp empty))
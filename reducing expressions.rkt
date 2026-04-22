#lang racket

(define-struct Var (id) #:transparent)
(define-struct App (fst snd) #:transparent)
(define-struct Abs (par bdy) #:transparent)

;takes in one lambda calculus s-expression, and returns the corresponding AST
;assumes all parentheses are written out, and that the lambda calculus expression is written with racket syntax
(define (parse sexp)
  (match sexp
    [`(λ (,x) ,y) (Abs x (parse y))]
    [`(lambda (,x) ,y) (Abs x (parse y))]

    ;; y is something
    ;; x is a list of parameters
    [`(λ (,x ...) ,y) (uncurry x y)]
    [`(lambda (,x ...) ,y) (uncurry x y)]
    
    [`(,x ,y) (App (parse x) (parse y))]

    ;; x is a call, y is a list of parameters
    [`(,x ,y ...) (app (parse x) (reverse y))]

    
    [x (Var x)]))

(define (uncurry x y)
  (cond
    [(empty? x) (parse y)]
    [else (Abs (first x) (uncurry (rest x) y))]))

(define (app x y)
  (cond
    [(empty? y) x]
    [else (App (app x (rest y)) (parse (first y)))]))

;;(define (app y)
;;  (cond
;;    [(empty? y) (parse y)]
;;    [else (App (first y)) (app (rest y)))]))

(define (subst par bdy exp)
  (match bdy
    [(Var id) (if (symbol=? id par) exp (Var id))]
    [(App fst snd) (App (subst par fst exp) (subst par snd exp))]
    [(Abs id bdy) (if (symbol=? id par) (Abs id bdy) (Abs id (subst par bdy exp)))]))
  

;;lambda-interp-head evaluates all redexes at the head level
;;the output is guarateed to not have a redex in head position
(define (lambda-interp-head ast)
  (match ast
    [(App (Abs par bdy) exp) (lambda-interp-head (subst par bdy exp))]
    [(App fst snd) (local
                     [(define interp-fst (lambda-interp-head fst))]
                     (if (Abs? interp-fst)
                       (lambda-interp-head (App interp-fst snd))
                       (App fst snd)))]
    [x x]))

;;assumes the input has no redex in head position, evaluates inner redexes
(define (lambda-interp-inner ast)
  (match ast
    [(App fst snd) (App (lambda-interp fst) (lambda-interp snd))]
    [(Abs par bdy) (Abs par (lambda-interp bdy))]
    [x x]))

(define (lambda-interp ast) (lambda-interp-inner (lambda-interp-head ast)))
 

;;printing function for testing purposes
(define (lambda-print ast)
  (match ast
    [(App fst snd) (list (lambda-print fst) (lambda-print snd))]
    [(Abs par bdy) (list 'λ (list par) (lambda-print bdy))]
    [(Var id) id]))

(define (uncurried-eval sexp)
  (Var-id (lambda-interp (parse sexp))))

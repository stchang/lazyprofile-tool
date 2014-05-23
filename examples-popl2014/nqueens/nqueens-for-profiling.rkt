#lang s-exp "../../lazy-profile.rkt"

(define (map f lst)
  (if (null? lst)
      null
      (cons (f (car lst)) (map f (cdr lst)))))

(define (rng n m)
  (if (>= n m)
      (list m)
      (cons n (rng (add1 n) m))))

(define (filter p? lst)
  (if (null? lst)
      null
      (let ([x (car lst)])
        (if (p? x)
            (cons x (filter p? (cdr lst)))
            (filter p? (cdr lst))))))

(define (foldl f acc lst)
  (if (null? lst)
      acc
      (foldl f (f (car lst) acc) (cdr lst))))

(define (foldr f base lst)
  (if (null? lst)
      base
      (f (car lst) (foldr f base (cdr lst)))))

(define (andmap f lst)
  (if (null? lst)
      true
      (and (f (car lst)) (andmap f (cdr lst)))))

(define (append lst1 lst2)
  (if (null? lst1)
      lst2
      (cons (car lst1) (append (cdr lst1) lst2))))

(define (tails lst)
  (if (null? lst)
      (list null)
      (cons lst (tails (cdr lst)))))

(define (length lst)
  (if (null? lst)
      0
      (add1 (length (cdr lst)))))

(define/prim (!= x y) (not (= x y)))
(define/prim (abs- x y) (if (< x y) (- y x) (- x y)))

(define/prim (safe? q1 q2)
  (and (!= (car q1) (car q2))
       (!= (cdr q1) (cdr q2))
       (!= (abs- (car q1) (car q2))
           (abs- (cdr q1) (cdr q2)))))

(define/prim (safe/lst? lst)
  (if (null? lst)
      true
      (andmap (λ (q) (safe? (car lst) q)) (cdr lst))))

(define/prim (valid? qs) (andmap safe/lst? (tails qs)))

(define (nqueens n)
  (let* ([process-row 
          (λ (r qss-so-far)
            (foldr
             (λ (qs new-qss)
               (append 
                (map 
                 (λ (c) (cons (cons r c) qs)) 
                 (rng 1 n))
                new-qss))
             null qss-so-far))]
         [all-possible-solutions (foldl process-row (list null) (rng 1 n))])
    (car (filter valid? all-possible-solutions))))

(nqueens 5)
  


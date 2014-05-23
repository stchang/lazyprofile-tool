#lang racket
(require (prefix-in r: (only-in racket cons car cdr null?)))

;; nqueens with streams instead of eager lists
;; - extra laziness in foldr
;; - force every list primitive

(define-syntax-rule (cons x y) (r:cons x (lazy y)))
(define-syntax-rule (car x) (r:car (force x)))
(define-syntax-rule (cdr x) (r:cdr (force x)))
(define-syntax-rule (null? x) (r:null? (force x)))

(define (build-list n f) 
  (define (build-list-help n f m)
    (if (= n m)
        null
        (cons (f m) (build-list-help n f (add1 m)))))
  (build-list-help n f 0))

(define (filter p? lst)
  (if (null? lst)
      null
      (let ([head (car lst)])
        (if (p? head)
            (cons head (filter p? (cdr lst)))
            (filter p? (cdr lst))))))

(define (foldl f acc lst)
  (if (null? lst)
      acc
      (foldl f (f (car lst) acc) (cdr lst))))

(define (andmap f lst)
  (if (null? lst)
      true
      (and (f (car lst)) (andmap f (cdr lst)))))

(define (append lst1 lst2)
  (if (null? lst1)
      lst2
      (cons (car lst1) (append (cdr lst1) lst2))))

(define (!= x y) (not (= x y)))
(define (abs- x y) (if (< x y) (- y x) (- x y)))
(define (safe? q1 q2)
  (and (!= (car q1) (car q2)) 
       (!= (cdr q1) (cdr q2))
       (!= (abs- (car q1) (car q2)) (abs- (cdr q1) (cdr q2)))))

(define (tails lst)
  (if (null? lst)
      (cons null null)
      (cons lst (tails (cdr lst)))))

(define (map f lst)
  (if (null? lst)
      null
      (cons (f (car lst)) (map f (cdr lst)))))

;; lazy foldr
(define (foldr f base lst)
  (if (null? lst)
      base
      (f (car lst) (lazy (foldr f base (cdr lst))))))


(define (nqueens n)
  (let ([qu 
         (λ (i qss) 
           (foldr
            (λ (qs acc)
              (append (map (λ (k) (cons (r:cons i k) qs))
                           (build-list n add1))
                      acc))
            null qss))]
        [ok? 
         (λ (lst) 
           (if (null? lst)
               true
               (andmap (λ (q) (safe? (car lst) q)) (cdr lst))))])
    (let ([all-possible-solns 
           (foldl qu (cons null null) (build-list n add1))]
          [valid? 
           (λ (lst) (andmap ok? (tails lst)))])
      (car (filter valid? all-possible-solns)))))


(define (force-list lst)
  (if (null? lst)
      null
      (r:cons (car lst) (force-list (cdr lst)))))

(define (show-queens res)
  (define resf (force-list res))
  (define n (length resf))
  (for ([i n])
    (for ([j n])
      (if (member (r:cons (add1 i) (add1 j)) resf)
          (printf "Q ")
          (printf "· ")))
    (printf "\n")))


(show-queens (time (nqueens 8)))

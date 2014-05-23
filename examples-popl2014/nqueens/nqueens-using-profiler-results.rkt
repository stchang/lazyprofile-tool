#lang racket
(require (prefix-in r: (only-in racket cons car cdr null?)))

;; nqueens with only lazy foldr and filter (follows profiler results)

(define-syntax-rule (lcons x y) (r:cons x (lazy y)))
(define-syntax-rule (fcar x) (r:car (force x)))
(define-syntax-rule (fcdr x) (r:cdr (force x)))
(define-syntax-rule (fnull? x) (r:null? (force x)))

;; lazy filter
(define (filter p? lst)
  (if (fnull? lst)
      null
      (let ([head (fcar lst)])
        (if (p? head)
            (lcons head (filter p? (fcdr lst)))
            (filter p? (fcdr lst))))))

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

;; lazy foldr
(define (foldr f base lst)
  (if (fnull? lst)
      base
      (f (fcar lst) (lazy (foldr f base (fcdr lst))))))


(define (nqueens n)
  (let ([qu 
         (λ (i qss) 
           (foldr
            (λ (qs acc)
              (append (map (λ (k) (cons (cons i k) qs))
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
      (cons (car lst) (force-list (cdr lst)))))

(define (show-queens res)
  (define resf (force-list res))
  (define n (length resf))
  (for ([i n])
    (for ([j n])
      (if (member (cons (add1 i) (add1 j)) resf)
          (printf "Q ")
          (printf "· ")))
    (printf "\n")))


(show-queens (time (nqueens 8)))

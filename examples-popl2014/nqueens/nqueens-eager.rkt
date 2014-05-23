#lang racket

;; nqueens, no laziness

(define (!= x y) (not (= x y)))
(define (abs- x y) (if (< x y) (- y x) (- x y)))
(define (safe? q1 q2)
  (and (and (!= (car q1) (car q2)) 
            (!= (cdr q1) (cdr q2)))
       (!= (abs- (car q1) (car q2)) (abs- (cdr q1) (cdr q2)))))

(define (tails lst)
  (if (null? lst)
      (cons null null)
      (cons lst (tails (cdr lst)))))

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
               (andmap (λ (q) (safe? (first lst) q)) (rest lst))))])
    (let ([all-possible-solns 
           (foldl qu (cons null null) (build-list n add1))]
          [valid? 
           (λ (lst) (andmap ok? (tails lst)))])
      (first (filter valid? all-possible-solns)))))


(define (show-queens res)
  (define n (length res))
  (for ([i n])
    (for ([j n])
      (if (member (cons (add1 i) (add1 j)) res)
          (printf "Q ")
          (printf "· ")))
    (printf "\n")))


(show-queens (time (nqueens 8)))

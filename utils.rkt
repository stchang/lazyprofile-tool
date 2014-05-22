#lang racket

(provide (all-defined-out))

(define (safe-/ x y) (if (zero? y) y (/ x y)))

(define (set-filter p? s) (for/set ([x s] #:when (p? x)) x))
(define (set-copy s) (for/set ([x (in-set s)]) x))

;; returns the index of element x in lst
;; ie (list-ref (list-index x lst) lst) = x
(define (list-index x lst [default #f])
  (let LOOP ([n 0] [lst lst])
    (if (null? lst)
        default
        (if (equal? (car lst) x)
            n
            (LOOP (add1 n) (cdr lst))))))


;; ----------------------------------------------------------------------------
;; -------- TESTS --------

(module+ test
  (require rackunit)
  (require srfi/26) ; for cut
  
  (define lst (list 4 5 6 7 8))
  (check-equal? (map
                 (cut list-ref lst <>)
                 (map (cut list-index <> lst) lst))
                lst)
  
  (check-eq? (list-index 10 (list 3 4 5) -1) -1)
  )
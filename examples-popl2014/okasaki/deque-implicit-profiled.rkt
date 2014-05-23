#lang s-exp "../../lazy-profile.rkt"

(struct Zero ())
(struct One (elem))

(struct Two (first second))

(struct Three (first second third))

;(define-type (D A)  (U Zero (One A) (Two A) (Three A)))

;(define-type (D1 A) (U Zero (One A) (Two A)))

(struct Shallow (elem))

(struct Deep (F M R))

;(define-type (Deque A) (U (Shallow A) (Deep A)))

;; An empty deque
(define empty (Shallow (Zero)))

;; Check for empty deque
;(: empty? : (All (A) ((Deque A) -> Boolean)))
(define (empty? que) (and (Shallow? que) (Zero? (Shallow-elem que))))

;; Inserts an element into the front of deque
;(: enqueue-front : (All (A) (A (Deque A) -> (Deque A))))
(define (enqueue-front elem que)
  (match que    
    [(Shallow (Zero)) (Shallow (One elem))]
    [(Shallow (One a)) (Shallow (Two elem a))]
    [(Shallow (Two a b)) 
     (Shallow (Three elem a b))]
    [(Shallow (Three f s t))
     (Deep (Two elem f) (cons empty empty) (Two s t))]
    [(Deep (Zero) m r) (Deep (One elem) m r)]
    [(Deep (One a) m r) (Deep (Two elem a) m r)]
    [(Deep (Two a b) m r) 
     (Deep (Three elem a b) m r)]
    [(Deep (Three f s t) m r)
     (Deep (Two elem f) 
           (let* ([forced-mid m]
                  [first (car forced-mid)]
                  [second (cdr forced-mid)])
             (cons (enqueue-front s first) (enqueue-front t second)))
           r)]))

;; Inserts into the rear of the queue
;(: enqueue : (All (A) (A (Deque A) -> (Deque A))))
(define (enqueue elem que)
  (match que    
    [(Shallow (Zero)) (Shallow (One elem))]
    [(Shallow (One a)) (Shallow (Two a elem))]
    [(Shallow (Two a b)) 
     (Shallow (Three a b elem))]
    [(Shallow (Three f s t))
     (Deep (Two f s) (cons empty empty) (Two t elem))]
    [(Deep f m (Zero)) (Deep f m (One elem))]
    [(Deep f m (One a)) (Deep f m (Two a elem))]
    [(Deep f m (Two a b)) 
     (Deep f m (Three a b elem))]
    [(Deep fi m (Three f s t)) 
     (Deep fi 
           (let* ([forced-mid m]
                  [first (car forced-mid)]
                  [second (cdr forced-mid)])
             (cons (enqueue f first) (enqueue s second)))
           (Two t elem))]))

;; Returns the first element of the deque
;(: head : (All (A) ((Deque A) -> A)))
(define (head que)
  (match que    
    [(Shallow (Zero)) (error 'head "given deque is empty")]
    [(Shallow (One f)) f]
    [(Shallow (Two f s)) f]
    [(Shallow (Three f s t)) f]
    [(Deep (Zero) m r) (error 'head "given deque is empty")]
    [(Deep (One f) m r) f]
    [(Deep (Two f s) m r) f]
    [(Deep (Three f s t) m r) f]))

;; Returns the last element of the deque 
;(: last : (All (A) ((Deque A) -> A)))
(define (last que)
  (match que    
    [(Shallow (Zero)) (error 'last "given deque is empty")]
    [(Shallow (One f)) f]
    [(Shallow (Two f s)) s]
    [(Shallow (Three f s t)) t]
    [(Deep f m (Zero)) (error 'last "given deque is empty")]
    [(Deep fi m (One f)) f]
    [(Deep fi m (Two f s)) s]
    [(Deep fi m (Three f s t)) t]))

;; Returns a deque without the first element
;(: tail : (All (A) ((Deque A) -> (Deque A))))
(define (tail que)
  (match que    
    [(Shallow (Zero)) (error 'tail "given deque is empty")]
    [(Shallow (One _)) (Shallow (Zero))]
    [(Shallow (Two _ s)) (Shallow (One s))]
    [(Shallow (Three _ s t)) (Shallow (Two s t))]
    [(Deep (Zero) m r) (error 'tail "given deque is empty")]
    [(Deep (One _) mid r) 
     (let ([m mid])
       (if (empty? (car m))
           (Shallow r)
           (let* ([carm (car m)]
                  [cdrm (cdr m)]
                  [first (head carm)]
                  [second (head cdrm)])
             (Deep (Two first second) 
                   (cons (tail carm) (tail cdrm)) r))))]
    [(Deep (Two _ s) m r) (Deep (One s) m r)]
    [(Deep (Three _ s t) m r) 
     (Deep (Two s t) m r)]))

;; Returns a deque without the last element
;(: init : (All (A) ((Deque A) -> (Deque A))))
(define (init que)
  (match que
    [(Shallow (Zero)) (error 'init "given deque is empty")]
    [(Shallow (One f)) (Shallow (Zero))]
    [(Shallow (Two f _)) 
     (Shallow (One f))]
    [(Shallow (Three f s _)) (Shallow (Two f s))]
    [(Deep f m (Zero)) (error 'init "given deque is empty")]
    [(Deep f mid (One a))
     (let* ([m mid]
            [carm (car m)])
       (if (empty? carm)
           (Shallow f)
           (let* ([cdrm (cdr m)]
                  [first (last carm)]
                  [second (last cdrm)])
             (Deep f (cons (init carm) (init cdrm)) 
                   (Two first second)))))]
    [(Deep fi m (Two f _)) (Deep fi m (One f))]
    [(Deep fi m (Three f s t)) 
     (Deep fi m (Two f s))]))



;; Similar to build-list
;(: build-deque : (All (A) (Natural (Natural -> A) -> (Deque A))))
(define (build-deque size func)
  (let loop ([n size])
        (if (zero? n)
            empty
            (let ([nsub1 (sub1 n)])
              (enqueue (func nsub1) (loop nsub1))))))
(define (build-deque-front size func)
  (let loop ([n size])
        (if (zero? n)
            empty
            (let ([nsub1 (sub1 n)])
              (enqueue-front (func nsub1) (loop nsub1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

;; suggests delaying ln77,col15 and ln76,col15 (maintainR)
;; - because we are inserting from rear
;(build-deque 1024 add1)

;; suggests delaying ln67,col15 and ln66,col15 (maintainF)
;; - because we are inserting from the front
#;(build-deque-front 1024 add1)

;; suggests delay in both maintainR and maintainF (F first), and in append
#;(let loop ([d (build-deque 1024 add1)] [n 0])
  (if (= n 50) 0
      (+ (head d) (loop (tail d) (add1 n)))))

; suggests delay in both maintainR and maintainF (R first) (no append)
#;(let loop ([d (build-deque 1024 add1)] [n 0])
  (if (= n 50) 0
      (+ (last d) (loop (init d) (add1 n)))))

; suggests delay in maintainF only (no append)
#;(let loop ([d (build-deque-front 1024 add1)] [n 0])
  (if (= n 50) 0
      (+ (head d) (loop (tail d) (add1 n)))))

; suggests delay in maintainF only, and in append
(let loop ([d (build-deque-front 256 add1)] [n 0])
  (if (= n 50) 0
      (+ (last d) (loop (init d) (add1 n)))))

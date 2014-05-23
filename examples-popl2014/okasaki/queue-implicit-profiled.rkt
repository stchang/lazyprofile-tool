#lang s-exp "../../lazy-profile.rkt"

;; BUG: uses pair of queues for Deep-mid (much slower!), instead of queue of pairs

(struct Zero ())
(struct One (elem))
;(struct Two (fst snd))
(struct Two (fst snd))

;(define-type (ZeroOne A) (U Zero (One A)))

;(define-type (OneTwo A) (U (One A) (Two A)))

(struct Shallow (elem))
(struct Deep (F M R))

;(define-type (Queue A) (U (Shallow A) (Deep A)))


;; An empty queue
(define empty (Shallow (Zero)))

;; Check for empty queue
;(: empty? : (All (A) ((Queue A) -> Boolean)))
(define (empty? que)
  (and (Shallow? que) (Zero? (Shallow-elem que))))

;; Inserts an element into the queue
;(: enqueue : (All (A) (A (Queue A) -> (Queue A))))
(define (enqueue elem que)
  (match que
    [(Shallow (Zero)) (Shallow (One elem))]
    [(Shallow (One one)) (Deep (Two one elem)
                               empty
                               (Zero))]
    [(Deep f m (Zero)) (Deep f m (One elem))]
    [(Deep f m (One el))
     (Deep f (enqueue (cons el elem) m) (Zero))]))

;; Returns the first element of the queue
;(: head : (All (A) ((Queue A) -> A)))
(define (head que)
  (match que
    [(Shallow (Zero)) (error 'head "given queue is empty")]
    [(Shallow (One one)) one]
    [(Deep (One one) m r) one]
    [(Deep (Two one two) m r) one]))

;; Returns the rest of the queue
;(: tail : (All (A) ((Queue A) -> (Queue A))))
(define (tail que)
  (match que
    [(Shallow (Zero)) (error 'tail "given queue is empty")]
    [(Shallow (One one)) (Shallow (Zero))]
    [(Deep (Two one two) m r) (Deep (One two) m r)]
    [(Deep f m r) 
     (let ([forced-mid m])
       (if (empty? forced-mid)
           (Shallow r)
           (let* ([p (head forced-mid)]
                  [fst (car p)]
                  [snd (cdr p)])
             (Deep (Two fst snd) (tail forced-mid) r))))]))


;; Returns the pair of first and the rest of the queue
;(: head+tail : (All (A) ((Queue A) -> (Pair A (Queue A)))))
(define (head+tail que) (cons (head que) (tail que)))

;; Similar to build-list function
;(: build-queue : (All (A) (Natural (Natural -> A) -> (Queue A))))
(define (build-queue size func)
  (let loop ([n size])
    (if (zero? n)
        empty
        (let ([nsub1 (sub1 n)])
          (enqueue (func nsub1) (loop nsub1))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

;; add first thresh elements of queue, with thresh increasing
;(build-queue 4097 add1)

(let loop ([q (build-queue 2047 add1)] [n 0])
  (if (>= n 20)
      0 
      (+ (head q) (loop (tail q) (add1 n)))))

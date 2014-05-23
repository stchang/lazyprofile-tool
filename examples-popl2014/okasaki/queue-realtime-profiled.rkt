#lang s-exp "../../lazy-profile.rkt"

(struct Queue (front rear scdul))


;; An empty queue
(define empty (Queue null null null))

;; Function to check for empty queue
;(: empty? : (All (A) ((Queue A) -> Boolean)))
(define (empty? rtq) (null? (Queue-front rtq)))


;(: rotate : (All (A) ((Stream A) (Listof A) (Stream A) -> (Stream A))))
(define (rotate frnt rer accum)
  (let ([carrer (car rer)])
    (if (null? frnt)
        (cons carrer accum)
        (cons (car frnt)
              (rotate (cdr frnt) 
                      (cdr rer) 
                      (cons carrer accum))))))

;(: internal-queue : (All (A) ((Stream A) (Listof A) (Stream A) -> (Queue A))))
(define (internal-queue front rear schdl)
  (if (null? schdl)
      (let ([newf (rotate front rear schdl)])
        (Queue newf null newf))
      (Queue front rear (cdr schdl))))


;(: enqueue : (All (A) (A (Queue A) -> (Queue A))))
(define (enqueue elem rtq)
  (internal-queue (Queue-front rtq)
                  (cons elem (Queue-rear rtq))
                  (Queue-scdul rtq)))


;(: head : (All (A) ((Queue A) -> A)))
(define (head rtq)
  (let ([front (Queue-front rtq)])
    (if (null? front)
        (error 'head "given queue is empty")
        (car front))))


;(: tail : (All (A) ((Queue A) -> (Queue A))))
(define (tail rtq)
  (let ([front (Queue-front rtq)])
    (if (null? front)
        (error 'tail "given queue is empty")
        (internal-queue (cdr front) 
                        (Queue-rear rtq) 
                        (Queue-scdul rtq)))))
  


;; Returns the pair of first and the rest of the queue
;(: head+tail : (All (A) ((Queue A) -> (Pair A (Queue A)))))
(define (head+tail que)
  (let ([front (Queue-front que)])
    (if (null? front)
        (error 'head+tail "given queue is empty")
        (cons (car front)
              (internal-queue (cdr front) 
                              (Queue-rear que) 
                              (Queue-scdul que))))))

;; Similar to build-list function
;(: build-queue : (All (A) (Natural (Natural -> A) -> (Queue A))))
(define (build-queue size func)
  (let loop ([n size])
        (if (zero? n)
            (Queue null null null) 
            (let ([nsub1 (sub1 n)])
              (enqueue (func nsub1) (loop nsub1))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(let ([qsize 1000])
  (let loop ([q (build-queue qsize add1)] [n 0])
    (if (= n 30) 0
        (+ (head q) (loop (tail q) (add1 n)))))
  #;(build-queue qsize add1))

;; notes
;; essentially matches okasaki's laziness, but
;; only if thresh is high enough, ie 20 (out of 1000) produces no suggestions
;; but 30 does
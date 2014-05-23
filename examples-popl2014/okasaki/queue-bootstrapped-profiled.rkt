;#lang racket
#lang s-exp "../../lazy-profile.rkt"

(define (reverse lst)
  (define (loop lst accum)
;  (let loop ([stream stream] [accum empty-stream])
    (if (null? lst)
        accum
        (loop (cdr lst) 
              (cons (car lst) accum))))
  (loop lst null))

(struct IntQue (F M LenFM R LenR))

;; An empty queue
(define empty null)

;(define-type (Queue A) (U Null (IntQue A)))

;; Checks for empty
;(: empty? : (All (A) ((Queue A) -> Boolean)))
(define (empty? bsq) (null? bsq))

;; Maintains invarients
;(: internal-queue : (All (A) ((Listof A) (Mid A) Integer (Listof A) Integer 
;                                         -> (Queue A))))
(define (internal-queue f m lenfm r lenr)
  (if (<= lenr lenfm) 
      (checkF (IntQue f m lenfm r lenr))
      (checkF (IntQue f (enqueue (reverse r) m) 
                      (+ lenfm lenr)
                      null 0))))

;; Inserts an element into the queue
;(: enqueue : (All (A) (A (Queue A) -> (Queue A))))
(define (enqueue elem bsq)
  (if (null? bsq)
      (IntQue (cons elem null) empty 1 null 0)
      (internal-queue (IntQue-F bsq)
                      (IntQue-M bsq)
                      (IntQue-LenFM bsq)
                      (cons elem (IntQue-R bsq))
                      (add1 (IntQue-LenR bsq)))))

;; Returns the first element of the queue
;(: head : (All (A) ((Queue A) -> A)))
(define (head bsq)
  (if (null? bsq)
      (error 'head "given queue is empty")
      (car (IntQue-F bsq))))

;; Returns the rest of the queue
;(: tail : (All (A) ((Queue A) -> (Queue A))))
(define (tail bsq)
  (if (null? bsq)
      (error 'tail "given queue is empty")
      (internal-queue (cdr (IntQue-F bsq)) 
                      (IntQue-M bsq) 
                      (sub1 (IntQue-LenFM bsq)) 
                      (IntQue-R bsq)
                      (IntQue-LenR bsq))))

;; Invarient check
;(: checkF : (All (A) ((IntQue A) -> (Queue A))))
(define (checkF que)
  (let* ([front (IntQue-F que)]
         [mid (IntQue-M que)])
    (if (null? front) 
        (if (empty? mid) 
            empty
            (IntQue (head mid)
                    (tail mid)
                    (IntQue-LenFM que)
                    (IntQue-R que)
                    (IntQue-LenR que)))
        que)))


;; Similar to build-list function
;(: build-queue : (All (A) (Natural (Natural -> A) -> (Queue A))))
(define (build-queue size func)
  (let loop ([n size])
        (if (zero? n)
            empty
            (let ([nsub1 (sub1 n)])
              (enqueue (func nsub1) (loop nsub1))))))


;; tests

;; add first X elements of queue
(let loop ([q (build-queue 1024 add1)] [n 0])
  (if (>= n 20)
      0 
      (+ (head q) (loop (tail q) (add1 n)))))



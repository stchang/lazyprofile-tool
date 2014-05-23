;#lang racket
#lang s-exp "../../lazy-profile.rkt"

;(require "../stream.rkt")

(define empty-stream null)
(define (append stream1 stream2)
  (cond
    [(null? stream1) stream2]
;    [(null? stream2) stream1]
    [else (cons (car stream1)
                (append (cdr stream1) stream2))]))
(define (reverse stream)
  (define (loop stream accum)
;  (let loop ([stream stream] [accum empty-stream])
    (if (null? stream)
        accum
        (loop (cdr stream) 
              (cons (car stream) accum))))
  (loop stream empty-stream))


;; A Banker's Queue (Maintains length of front >= length of rear)

(struct Queue (front lenf rear lenr))


;; An empty queue
(define empty (Queue empty-stream 0 empty-stream 0))

;; Checks if the given queue is empty
(define (empty? que) (zero? (Queue-lenf que)))


;; A Pseudo-constructor. Maintains the invariant lenf >= lenr
(define (internal-queue front lenf rear lenr)
  (if (>= lenf lenr)
      (Queue front lenf rear lenr)
      (Queue (append front (reverse rear))
             (+ lenf lenr)
             empty-stream 0)))

;; Pushes an element into the queue
(define (enqueue elem que)
  (internal-queue (Queue-front que) 
                  (Queue-lenf que) 
                  (cons elem (Queue-rear que))
                  (add1 (Queue-lenr que))))

;; Retrieves the head element of the queue
(define (head que)
  (if (zero? (Queue-lenf que))
      (error 'head "given queue is empty")
      (car (Queue-front que))))

;; Queueue operation. Removes the head and returns the rest of the queue
(define (tail que)
  (let ([lenf (Queue-lenf que)])
    (if (zero? lenf)
        (error 'tail "given queue is empty")
        (internal-queue (cdr (Queue-front que))
                        (sub1 lenf)
                        (Queue-rear que)
                        (Queue-lenr que)))))


;; A Queue constructor with the given element
(define (queue . lst)
  (foldl enqueue
         (Queue empty-stream 0 empty-stream 0)
         lst))

;; Similar to build-list function
(define (build-queue size func)
  (let loop ([n size])
        (if (zero? n)
            (Queue empty-stream 0 empty-stream 0)
            (let ([nsub1 (sub1 n)])
              (enqueue (func nsub1) (loop nsub1))))))

;; Returns pair of the first element of the queue and the rest 
;; of the queue
(define (head+tail que)
  (let ([lenf (Queue-lenf que)])
    (if (zero? lenf)
        (error 'head+tail "given queue is empty")
        (let ([front (Queue-front que)])
          (cons (car front) 
                (internal-queue (cdr front)
                                (sub1 lenf)
                                (Queue-rear que)
                                (Queue-lenr que)))))))


;; tests

;; add first X elements of queue
(let loop ([q (build-queue 1024 add1)] [n 0])
  (if (>= n 200)
      0 
      (+ (head q) (loop (tail q) (add1 n)))))
      
;; testing persistence -- using reversed list
;; 1) build queue of size 2^n - 1, so lenr = 0
;; 2) take tail 2^(n-1) - 1 times (and discard)
;; 3) take tail at 2^(n-1)th element of queue many times to "use" the reversed list
;; 4) see what the usage info says
#;(let ([q (build-queue 1023 add1)]
      [sum-times 10]) ; num times to call tail
;  (let outer-LOOP ([m 1])
       (let loop1 ([q q] [n 0] [n-tail 511])
         (if (< n n-tail)
             (loop1 (tail q) (add1 n) n-tail)
             (let loop2 ([sum 0] [m sum-times])
               (if (zero? m)
                   sum
                   (loop2 (+ (head (tail q)) sum) (sub1 m))))))
  )

;; testing delaying of rotation
;; 1) build queue of size (2^n)-2 (ie, right before reshuffling)
;; 2) so now front = rear, so a tail will cause resuffling
;; 3) repeat tail operation lots of times to see if persistent
#;(let ([q (build-queue 1022 add1)]
      [m 1000]) ; num times to call tail
  (let loop ([sum 0] [n m])
         (if (zero? n)
             sum
             (loop (+ (head (tail q)) sum) (sub1 n))))
  )

;; the most basic test to see whether (reverse rear) should be delayed
;(tail (build-queue 1023 add1))
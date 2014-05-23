#lang s-exp "../../lazy-profile.rkt"

;; A Banker's Deque (Maintains length of front >= length of rear)

(struct Deque (front lenf rear lenr))

(define inv-c 2)

(define empty-stream null)
(define (empty-stream? x) (null? x))
(define (stream-cons x y) (cons x y))
(define (stream-car x) (car x))
(define (stream-cdr x) (cdr x))
(define (stream-append stream1 stream2)
  (cond
    [(null? stream1) stream2]
;    [(null? stream2) stream1]
    [else (cons (car stream1)
                (stream-append (cdr stream1) stream2))]))
(define (stream-reverse stream)
  (define (loop stream accum)
    (if (null? stream)
        accum
        (loop (cdr stream) 
              (cons (car stream) accum))))
  (loop stream empty-stream))

(define (drop num stream)
  (cond 
    [(zero? num)    stream] 
    [(null? stream) (error 'drop "not enough elements to drop")]
    [else (drop (sub1 num) (cdr stream))]))

(define (take num stream)
  (cond 
    [(zero? num)    empty-stream]
    [(null? stream) (error 'take "not enough elements to take")]
    [else (stream-cons (car stream) (take (sub1 num) (cdr stream)))]))


(define empty (Deque empty-stream 0 empty-stream 0))

;; Checks if the given deque is empty
;(: empty? : (All (A) ((Deque A) -> Boolean)))
(define (empty? que)
  (zero? (+ (Deque-lenf que) (Deque-lenr que))))


;; A Pseudo-constructor. Maintains the invariants 
;; 1. lenf <= inv-c * lenr
;; 2. lenr <= inv-c * lenf
;(: internal-deque : 
;   (All (A) ((Stream A) Integer (Stream A) Integer -> (Deque A))))
(define (internal-deque front lenf rear lenr)
  (cond 
    [(> lenf (add1 (* lenr inv-c))) (maintainF front lenf rear lenr)]
    [(> lenr (add1 (* lenf inv-c))) (maintainR front lenf rear lenr)]
    [else (Deque front lenf rear lenr)]))


;; Maintains invariant lenf <= inv-c * lenr
;(: maintainF : (All (A) ((Stream A) Integer (Stream A) Integer -> (Deque A))))
(define (maintainF front lenf rear lenr)
  (let* ([new-lenf (arithmetic-shift (+ lenf lenr) -1)]
         [new-lenr (- (+ lenf lenr) new-lenf)]
         [newF (take new-lenf front)]
         [newR (stream-append rear (stream-reverse (drop new-lenf front)))])
    (Deque newF new-lenf newR new-lenr)))


;; Maintains invariant lenr <= inv-c * lenf
;(: maintainR : (All (A) ((Stream A) Integer (Stream A) Integer -> (Deque A))))
(define (maintainR front lenf rear lenr)
  (let* ([new-lenf (arithmetic-shift (+ lenf lenr) -1)]
         [new-lenr (- (+ lenf lenr) new-lenf)]
         [newR (take new-lenr rear)]
         [newF (stream-append front (stream-reverse (drop new-lenr rear)))])
    (Deque newF new-lenf newR new-lenr)))


;; Pushes an element into the Deque at the front end
;(: enqueue-front : (All (A) (A (Deque A) -> (Deque A))))
(define (enqueue-front elem deq)
  (internal-deque (stream-cons elem (Deque-front deq))
                  (add1 (Deque-lenf deq))
                  (Deque-rear deq)
                  (Deque-lenr deq)))


;; Pushes an element into the Deque at the rear end
;(: enqueue : (All (A) (A (Deque A) -> (Deque A))))
(define (enqueue elem deq)
  (internal-deque (Deque-front deq)
                  (Deque-lenf deq)
                  (stream-cons elem (Deque-rear deq))
                  (add1 (Deque-lenr deq))))

;; Retrieves the head element of the queue
;(: head : (All (A) ((Deque A) -> A)))
(define (head deq)
  (let ([lenf (Deque-lenf deq)]
        [lenr (Deque-lenr deq)])
    (if (zero? (+ lenf lenr))
        (error 'head "given deque is empty")
        (let ([front (Deque-front deq)])
          (if (empty-stream? front) 
              (stream-car (Deque-rear deq))
              (stream-car front))))))


;; Retrieves the last element of the queue
;(: last : (All (A) ((Deque A) -> A)))
(define (last deq)
  (if (zero? (+ (Deque-lenf deq) (Deque-lenr deq)))
      (error 'last "given deque is empty")
      (let ([rear (Deque-rear deq)])
        (if (empty-stream? rear) 
            (stream-car (Deque-front deq))
            (stream-car rear)))))

;; Dequeue operation. Removes the head and returns the rest of the queue
;(: tail : (All (A) ((Deque A) -> (Deque A))))
(define (tail deq)
  (let ([lenf (Deque-lenf deq)]
        [lenr (Deque-lenr deq)])
    (if (zero? (+ lenf lenr))
        (error 'tail "given deque is empty")
        (let ([front (Deque-front deq)])
          (if (empty-stream? front) 
              empty
              (internal-deque (stream-cdr front) 
                              (sub1 lenf)
                              (Deque-rear deq)
                              lenr))))))
  
;; Removes the last and returns the deque without the last
;(: init : (All (A) ((Deque A) -> (Deque A))))
(define (init deq)
  (let ([lenf (Deque-lenf deq)]
        [lenr (Deque-lenr deq)])
    (if (zero? (+ lenf lenr))
        (error 'init "given deque is empty")
        (let ([rear (Deque-rear deq)])
          (if (empty-stream? rear)
              empty
              (internal-deque (Deque-front deq) 
                              lenf
                              (stream-cdr rear)
                              (sub1 lenr)))))))



;; Similar to build-list function
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

;; Returns the pair head and tail of the given queue
;(: head+tail : (All (A) (Deque A) -> (Pair A (Deque A))))
(define (head+tail deq)
  (let ([lenf (Deque-lenf deq)]
        [lenr (Deque-lenr deq)])
    (if (zero? (+ lenf lenr))
        (error 'head+tail "given deque is empty")
        (let ([front (Deque-front deq)])
          (if (empty-stream? front) 
              (cons (stream-car (Deque-rear deq)) empty)
              (cons (stream-car front)
                    (internal-deque (stream-cdr front) 
                                    (sub1 lenf)
                                    (Deque-rear deq)
                                    lenr)))))))

;; Returns the pair last and init of the given queue
;(: last+init : (All (A) (Deque A) -> (Pair A (Deque A))))
(define (last+init deq)
  (let ([lenf (Deque-lenf deq)]
        [lenr (Deque-lenr deq)])
    (if (zero? (+ lenf lenr))
        (error 'last+init "given deque is empty")
        (let ([rear (Deque-rear deq)])
          (if (empty-stream? rear)
              (cons (stream-car (Deque-front deq)) empty)
              (cons (stream-car rear)
                    (internal-deque (Deque-front deq) 
                                    lenf
                                    (stream-cdr rear)
                                    (sub1 lenr))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

;; suggests delaying ln77,col15 and ln76,col15 (maintainR)
;; - because we are inserting from rear
;(build-deque 1024 add1)

;; suggests delaying ln67,col15 and ln66,col15 (maintainF)
;; - because we are inserting from the front
(build-deque-front 128 add1)

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
(let loop ([d (build-deque-front 1024 add1)] [n 0])
  (if (= n 50) 0
      (+ (last d) (loop (init d) (add1 n)))))
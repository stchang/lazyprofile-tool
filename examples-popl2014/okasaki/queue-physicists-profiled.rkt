#lang s-exp "../../lazy-profile.rkt"

(define (append lst1 lst2)
  (cond
    [(null? lst1) lst2]
;    [(null? stream2) stream1]
    [else (cons (car lst1) (append (cdr lst1) lst2))]))
(define (reverse lst)
  (define (loop lst accum)
;  (let loop ([stream stream] [accum empty-stream])
    (if (null? lst)
        accum
        (loop (cdr lst) 
              (cons (car lst) accum))))
  (loop lst null))

;; Physicists Queue
;; Maintains invariant lenr <= lenf
;; pref is empty only if lenf = 0
(struct Queue (preF front lenf rear lenr))

;; Empty Queue Because of some limitations in TR in typechecking the
;; boxed values, I had to make empty a macro so that users can
;; instantiate empty easily.
(define empty (Queue null null 0 null 0))

;; Checks if the given Queue is empty
(define (empty? que) (zero? (Queue-lenf que)))

;; Maintains "preF" invariant (preF in not not null when front is not null)
;(: check-pref-inv : 
;   (All (A) ((Listof A) (Promiseof A) Integer (Listof A) Integer ->
;                        (Queue A))))
(define (check-pref-inv pref front lenf rear lenr)
  (if (null? pref)
      (Queue front front lenf rear lenr)
      (Queue pref front lenf rear lenr)))


;; Maintains lenr <= lenf invariant
;(: check-len-inv : 
;   (All (A) ((Listof A) (Promiseof A) Integer (Listof A) Integer -> (Queue A))))
(define (check-len-inv pref front lenf rear lenr)
  (if (>= lenf lenr)
      (check-pref-inv pref front lenf rear lenr)
      (let* ([newpref front]
             [newf (append newpref (reverse rear))])
             (check-pref-inv newpref newf (+ lenf lenr) null 0))))

;; Maintains queue invariants
;(: internal-queue : 
;   (All (A) ((Listof A) (Promiseof A) Integer (Listof A) Integer -> (Queue A))))
(define (internal-queue pref front lenf rear lenr)
  (check-len-inv pref front lenf rear lenr))

;; Enqueues an item into the list
(define (enqueue item que)
  (internal-queue (Queue-preF que)
                  (Queue-front que)
                  (Queue-lenf que)
                  (cons item (Queue-rear que))
                  (add1 (Queue-lenr que))))


;; Returns the first element in the queue if non empty. Else raises an error
(define (head que)
  (if (zero? (Queue-lenf que))
      (error 'head "given queue is empty")
      (car (Queue-preF que))))


;; Removes the first element in the queue and returns the rest
(define (tail que)
  (let ([lenf (Queue-lenf que)])
    (if (zero? lenf)
        (error 'tail "given queue is empty")
        (internal-queue (cdr (Queue-preF que))
                        (cdr (Queue-front que))
                        (sub1 lenf)
                        (Queue-rear que)
                        (Queue-lenr que)))))


(define (head+tail que)
  (let ([lenf (Queue-lenf que)])
    (if (zero? lenf)
        (error 'head+tail "given queue is empty")
        (let ([pref (Queue-preF que)])
          (cons (car pref)
                (internal-queue
                 (cdr pref) 
                 (cdr (Queue-front que))
                 (sub1 lenf) 
                 (Queue-rear que) 
                 (Queue-lenr que)))))))


(define (build-queue size func)
  (let loop ([n size])
    (if (zero? n)
        (Queue null null 0 null 0)
        (let ([nsub1 (sub1 n)])
          (enqueue (func nsub1) (loop nsub1))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

;; tests

;; add first X elements of queue
(let loop ([q (build-queue 1024 add1)] [n 0])
  (if (>= n 200)
      0 
      (+ (head q) (loop (tail q) (add1 n)))))
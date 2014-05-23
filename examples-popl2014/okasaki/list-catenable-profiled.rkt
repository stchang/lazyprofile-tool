;#lang racket
#lang s-exp "../../lazy-profile.rkt"



;; bootstrapped queue (profiled)


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
(define bsq:empty null)

;(define-type (Queue A) (U Null (IntQue A)))

;; Checks for empty
;(: empty? : (All (A) ((Queue A) -> Boolean)))
(define (bsq:empty? bsq) (null? bsq))

;; Maintains invarients
;(: internal-queue : (All (A) ((Listof A) (Mid A) Integer (Listof A) Integer 
;                                         -> (Queue A))))
(define (internal-queue f m lenfm r lenr)
  (if (<= lenr lenfm) 
      (checkF (IntQue f m lenfm r lenr))
      (checkF (IntQue f (bsq:enqueue (reverse r) m) 
                      (+ lenfm lenr)
                      null 0))))

;; Inserts an element into the queue
;(: enqueue : (All (A) (A (Queue A) -> (Queue A))))
(define (bsq:enqueue elem bsq)
  (if (null? bsq)
      (IntQue (cons elem null) bsq:empty 1 null 0)
      (internal-queue (IntQue-F bsq)
                      (IntQue-M bsq)
                      (IntQue-LenFM bsq)
                      (cons elem (IntQue-R bsq))
                      (add1 (IntQue-LenR bsq)))))

;; Returns the first element of the queue
;(: head : (All (A) ((Queue A) -> A)))
(define (bsq:head bsq)
  (if (null? bsq)
      (error 'head "given queue is empty")
      (car (IntQue-F bsq))))

;; Returns the rest of the queue
;(: tail : (All (A) ((Queue A) -> (Queue A))))
(define (bsq:tail bsq)
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
        (if (bsq:empty? mid) 
            bsq:empty
            (IntQue (head mid)
                    (tail mid)
                    (IntQue-LenFM que)
                    (IntQue-R que)
                    (IntQue-LenR que)))
        que)))


;; Similar to build-list function
;(: build-queue : (All (A) (Natural (Natural -> A) -> (Queue A))))
#;(define (build-queue size func)
  (let loop ([n size])
        (if (zero? n)
            bsq:empty
            (let ([nsub1 (sub1 n)])
              (bsq:enqueue (func nsub1) (loop nsub1))))))











(struct List (elem ques))

;(define-type (CatenableList A) (U (List A) Null))

;; An empty list
(define empty null)

;; Checks for empty list
;(: empty? : (All (A) ((CatenableList A) -> Boolean)))
(define (empty? cat) (null? cat))

;(: link : (All (A) ((List A) (Promise (List A)) -> (List A))))
(define (link lst cat)
  (List (List-elem lst) (bsq:enqueue cat (List-ques lst))))

;(: link-all : (All (A) ((bsq:Queue (Promise (List A))) -> (List A))))
(define (link-all rtq)
  (let ([hd (bsq:head rtq)]
        [tl (bsq:tail rtq)])
    (if (bsq:empty? tl)
        hd
        (link hd (link-all tl)))))

;; Append helper 
#;(: append-inner :
   (All (A) ((CatenableList A) (CatenableList A) -> (CatenableList A))))
(define (append-inner cat1 cat2)
  (cond
    [(null? cat1) cat2]
    [(null? cat2) cat1]
    [else (link cat1 cat2)]))

;; List append
;(: append : (All (A) ((CatenableList A) * -> (CatenableList A))))
#;(define (append . cats)
  (if (null? cats)
      empty
      (append-inner (car cats) (apply append (cdr cats)))))

;; Similar to list cons function
;(: kons : (All (A) (A (CatenableList A) -> (CatenableList A))))
(define (kons elem cat)
  (append-inner (List elem bsq:empty) cat))

;; Inserts an element at the rear end of the list
;(: kons-rear : (All (A) (A (CatenableList A) -> (CatenableList A))))
(define (kons-rear elem cat)
  (append-inner cat (List elem bsq:empty)))

;; Similar to list car function
;(: head : (All (A) ((CatenableList A) -> A)))
(define (head cat)
  (if (null? cat)
      (error 'first "given list is empty")
      (List-elem cat)))

;; Similar to list cdr function
;(: tail : (All (A) ((CatenableList A) -> (CatenableList A))))
(define (tail cat)
  (if (null? cat)
      (error 'rest "given list is empty")
      (tail-helper cat)))

;(: tail-helper : (All (A) ((List A) -> (CatenableList A))))
(define (tail-helper cat)
  (let ([ques (List-ques cat)])
    (if (bsq:empty? ques)
        empty
        (link-all ques))))

;; similar to list map function. apply is expensive so using case-lambda
;; in order to saperate the more common case
 

;; Similar to list reverse function
;(: reverse : (All (A) ((CatenableList A) -> (CatenableList A))))
#;(define (reverse list)
  #;(: local-reverse : (All (A) ((CatenableList A) (CatenableList A)
                                                 ->
                                                 (CatenableList A))))
  (define (local-reverse list accum)
    (if (empty? list)
        accum
        (local-reverse (tail list) (kons (head list) accum))))
  (local-reverse list empty))



;; Similar to build-list function of racket list
;(: build-list : (All (A) (Natural (Natural -> A) -> (CatenableList A))))
(define (build-list size func)
  (let loop ([n size] [accum empty])
        (if (zero? n)
            accum
            (loop (sub1 n) (kons (func (sub1 n)) accum)))))

;; Similar to make-list function of racket list
;(: make-list : (All (A) (Natural A -> (CatenableList A))))
(define (make-list size elem)
  (let loop ([n size] [accum empty])
        (if (zero? n)
            accum
            (loop (sub1 n) (kons elem accum)))))



;; tests

;; add first X elements of list
(let loop ([q (build-list 4096 add1)] [n 0])
  (if (>= n 100)
      0 
      (+ (head q) (loop (tail q) (add1 n)))))
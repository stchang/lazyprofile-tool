#lang s-exp "../../lazy-profile.rkt"

;(require scheme/match)

(struct IntHeap (elem heap lazy))

;(define-type (PHeap A) (U Null (IntHeap A)))

(struct PairingHeap (comparer heap))

;(define-type (Heap A) (PairingHeap A))

;; Checks for empty
;(: empty? : (All (A) ((PairingHeap A) -> Boolean)))
(define (empty? pheap) (null? (PairingHeap-heap pheap)))

;; An empty heap
(define empty null)

;; Merges two given heaps
;(: merge : (All (A) ((PairingHeap A) (PairingHeap A) -> (PairingHeap A))))
(define (merge pheap1 pheap2)
  (let ([func (PairingHeap-comparer pheap1)]
        [heap1 (PairingHeap-heap pheap1)]
        [heap2 (PairingHeap-heap pheap2)])
    (PairingHeap func (merge-help heap1 heap2 func))))

;; Helper for merge
;(: merge-help : (All (A) ((PHeap A) (PHeap A) (A A -> Boolean) -> (PHeap A))))
(define (merge-help pheap1 pheap2 func)
  (if (null? pheap1) pheap2
      (if (null? pheap2) pheap1
          (if (func (IntHeap-elem pheap1) (IntHeap-elem pheap2))
              (link pheap1 pheap2 func)
              (link pheap2 pheap1 func))))
  #;(match (cons pheap1 pheap2)
    [(cons null _) pheap2]
    [(cons _ null) pheap1]
    [(cons (IntHeap x _ _) #;(and h1 (IntHeap x _ _)) (IntHeap y _ _) #;(and h2 (IntHeap y _ _))) 
     (if (func x y)
;         (link h1 h2 func)
;         (link h2 h1 func))]))
         (link pheap1 pheap2 func)
         (link pheap2 pheap1 func))]))

;(: link : (All (A) ((IntHeap A) (IntHeap A) (A A -> Boolean) -> (PHeap A))))
(define (link heap1 heap2 func)
  (if (null? (IntHeap-heap heap1)) (IntHeap (IntHeap-elem heap1) heap2 (IntHeap-lazy heap1))
      (IntHeap (IntHeap-elem heap1) null
               (merge-help (merge-help heap2 (IntHeap-heap heap1) func)
                           (IntHeap-lazy heap1)
                           func)))
  #;(match heap1
    [(IntHeap x null m) (IntHeap x heap2 m)]
    [(IntHeap x b m) 
;     (IntHeap x '() (delay (merge-help (merge-help heap2 b func)
     (IntHeap x null (merge-help (merge-help heap2 b func)
;                                       (force m)
                                       m
                                      func))]))

;; Inserts an element into the heap
;(: insert : (All (A) (A (PairingHeap A) -> (PairingHeap A))))
(define (insert elem pheap)
  (let ([func (PairingHeap-comparer pheap)]
        [heap (PairingHeap-heap pheap)])
    (PairingHeap func (merge-help (IntHeap elem null
;                                           (delay '()))
                                           null)
                                  heap func))))

;; Returns min or max element of the heap
;(: find-min/max : (All (A) ((PairingHeap A) -> A)))
(define (find-min/max pheap)
  (let ([heap (PairingHeap-heap pheap)])
    (if (null? heap)
        (error 'find-min/max "given heap is empty")
        (IntHeap-elem heap))))

;; Deletes min or max element of the heap
;(: delete-min/max : (All (A) ((PairingHeap A) -> (PairingHeap A))))
(define (delete-min/max pheap)
  (let ([heap (PairingHeap-heap pheap)]
        [func (PairingHeap-comparer pheap)])
    (if (null? heap)
        (error 'delete-min/max "given heap is empty")
        (PairingHeap func (merge-help (IntHeap-heap heap) 
;                                      (force (IntHeap-lazy heap)) 
                                      (IntHeap-lazy heap) 
                                      func)))))

;; Heap constructor
;(: heap : (All (A) ((A A -> Boolean) A * -> (PairingHeap A))))
(define (heap comparer . lst)
  (let ([first (PairingHeap comparer null)])
    (foldl insert first lst)))


;(: sorted-list : (All (A) ((PairingHeap A) -> (Listof A))))
(define (sorted-list pheap)
  (let ([heap (PairingHeap-heap pheap)])
    (if (null? heap)
        null
        (cons (find-min/max pheap) (sorted-list (delete-min/max pheap))))))



;; Similar to build-list
;(: build-heap : (All (A) (Natural (Natural -> A) (A A -> Boolean) -> (Heap A))))
(define (build-heap size func comparer)
  (let loop ([n size])
        (if (zero? n)
            (PairingHeap comparer null)
            (let ([nsub1 (sub1 n)])
              (insert (func nsub1) (loop nsub1))))))

;; merge 2 pheaps, then sum first X elements
;; test notes: finds same lazy annotations as okasaki
;;             - in link, around the merge-help merge-help expr
(let ([hsize 200])
  (let loop ([h (merge (build-heap hsize add1 >)
                       (build-heap hsize add1 <))] [n 0])
    (if (= n 10) 0
        (+ (find-min/max h) (loop (delete-min/max h) (add1 n))))))


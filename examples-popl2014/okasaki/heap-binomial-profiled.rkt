#lang s-exp "../../lazy-profile.rkt"

(define (reverse lst)
  (define (loop lst accum)
;  (let loop ([stream stream] [accum empty-stream])
    (if (null? lst)
        accum
        (loop (cdr lst) (cons (car lst) accum))))
  (loop lst null))

(struct Node (rank val trees))

(struct Heap (comparer trees))

;; Checks for empty heap
(define (empty? heap) (null? (Heap-trees heap)))


;; merges two given nodes
;(: link : (All (A) ((Node A) (Node A) (A A -> Boolean) -> (Node A))))
(define (link node1 node2 func)
  (let ([val1  (Node-val node1)]
        [val2  (Node-val node2)]
        [rank1 (add1 (Node-rank node1))])
    (if (func val1 val2)
        (Node rank1 val1 (cons node2 (Node-trees node1)))
        (Node rank1 val2 (cons node1 (Node-trees node2))))))



;; Inserts a node into the tree
;; no null test?
(define (ins-tree node trees comparer)
  (let ([first (car trees)])
    (if (< (Node-rank node) (Node-rank first))
        (Heap comparer (cons node trees))
        (let ([rest (cdr trees)]
              [new  (link node first comparer)])
          (if (null? rest)
              (Heap comparer (list new))
              (ins-tree new rest comparer))))))

;; Inserts an element into the heap
(define (insert val heap)
  (let ([new-node (Node 0 val null)]
        [comparer (Heap-comparer heap)]
        [trees    (Heap-trees heap)])
    (if (null? trees)
        (Heap comparer (list new-node))
        (ins-tree new-node trees comparer))))


;; Merges two given heaps
(define (merge heap1 heap2)
  (let ([heap1-trees (Heap-trees heap1)]
        [heap2-trees (Heap-trees heap2)])
    (cond
      [(null? heap2-trees) heap1]
      [(null? heap1-trees) heap2]
      [else (merge-helper heap1-trees
                          heap2-trees
                          (Heap-comparer heap1))])))

;; Helper for merge
(define (merge-helper heap1-trees heap2-trees comp)
  (let* ([first-tree1 (car heap1-trees)]
         [first-tree2 (car heap2-trees)]
         [heap1       (Heap comp (cdr heap1-trees))]
         [heap2       (Heap comp (cdr heap2-trees))]
         [rank1       (Node-rank first-tree1)]
         [rank2       (Node-rank first-tree2)])
    (cond
      [(< rank1 rank2)
       (Heap comp (cons first-tree1
                        (Heap-trees (merge heap1 (Heap comp heap2-trees)))))]
      [(> rank1 rank2)
       (Heap comp (cons first-tree2
                        (Heap-trees (merge (Heap comp heap1-trees) heap2))))]
      [else (let ([rest (Heap-trees (merge heap1 heap2))]
                  [new  (link first-tree1 first-tree2 comp)])
               (if (null? rest)
                   (Heap comp (list new))
                   (ins-tree new rest comp)))])))

;; Returns the min element if min-heap else returns the max element 
(define (find-min/max heap)
  (let ([trees (Heap-trees heap)])
    (cond
      [(null? trees)       (error 'find-min/max "given heap is empty")]
      [(null? (cdr trees)) (Node-val (car trees))]
      [else (let* ([comparer (Heap-comparer heap)]
                   [x (Node-val (car trees))]
                   [y (find-min/max (Heap comparer (cdr trees)))])
               (if (comparer x y) x y))])))

;; Deletes min or max element (depends on min or max heap) 
(define (delete-min/max heap)
  (define (get-min trees func)
    (let ([first (car trees)]
          [rest  (cdr trees)]
          [heap  (Heap func trees)])
      (if (null? rest)
          heap
          (let* ([min-trees  (Heap-trees (get-min rest func))]
                 [first-tree (car min-trees)])
            (if (func (Node-val first) (Node-val first-tree))
                heap
                (Heap func (cons first-tree (cons first (cdr min-trees)))))))))
  (if (null? (Heap-trees heap))
      (error 'delete-min/max "given heap is empty")
      (let* ([func (Heap-comparer heap)]
             [newpair (get-min (Heap-trees heap) func)]
             [newpair-trees (Heap-trees newpair)])
         (merge (Heap func (reverse (Node-trees (car newpair-trees))))
                (Heap func (cdr newpair-trees))))))

;; Returns a sorted list (sorting depends on min or max heap)
;(: sorted-list : (All (A) ((Heap A) -> (Listof A))))
(define (sorted-list heap)
  (if (empty? heap)
      null
      (cons (find-min/max heap)
            (sorted-list (delete-min/max heap)))))





;; Similar to build-list
;(: build-heap : (All (A) (Natural (Natural -> A) (A A -> Boolean) -> (Heap A))))
(define (build-heap size func comparer)
  (let loop ([n size])
        (if (zero? n)
            (Heap comparer empty)
            (let ([nsub1 (sub1 n)])
              (insert (func nsub1) (loop nsub1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests


;(build-heap 5000 add1 >)
(let loop ([h (build-heap 1024 add1 <)] [n 0])
    (if (= n 10) 0
        (+ (find-min/max h) (loop (delete-min/max h) (add1 n)))))
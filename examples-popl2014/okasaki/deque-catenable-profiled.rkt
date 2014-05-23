#lang s-exp "../../lazy-profile.rkt"

;; deque-implicit-strict

(struct d:Zero ())
(struct d:One (elem))

(struct d:Two (first second))

(struct d:Three (first second third))

(struct d:Shallow (elem))

(struct d:Deep (F M R))

;; An empty deque
(define d:empty (d:Shallow (d:Zero)))

;; Check for empty deque
;(: empty? : (All (A) ((Deque A) -> Boolean)))
(define (d:empty? que) (and (d:Shallow? que) (d:Zero? (d:Shallow-elem que))))

;; Inserts an element into the front of deque, ie the "left", ie cons from Okasaki
;(: enqueue-front : (All (A) (A (Deque A) -> (Deque A))))
(define (d:enqueue-front elem que)
  (match que    
    [(d:Shallow (d:Zero))        (d:Shallow (d:One elem))]
    [(d:Shallow (d:One a))       (d:Shallow (d:Two elem a))]
    [(d:Shallow (d:Two a b))     (d:Shallow (d:Three elem a b))]
    [(d:Shallow (d:Three f s t)) (d:Deep (d:Two elem f) d:empty (d:Two s t))]
    [(d:Deep (d:Zero) m r)       (d:Deep (d:One elem) m r)]
    [(d:Deep (d:One a) m r)      (d:Deep (d:Two elem a) m r)]
    [(d:Deep (d:Two a b) m r)    (d:Deep (d:Three elem a b) m r)]
    [(d:Deep (d:Three f s t) m r) 
     (d:Deep (d:Two elem f) (d:enqueue-front (cons s t) m) r)]))

;; Inserts into the rear of the queue, ie the "right", ie snoc from Okasaki
;(: enqueue : (All (A) (A (Deque A) -> (Deque A))))
(define (d:enqueue elem que)
  (match que    
    [(d:Shallow (d:Zero))        (d:Shallow (d:One elem))]
    [(d:Shallow (d:One a))       (d:Shallow (d:Two a elem))]
    [(d:Shallow (d:Two a b))     (d:Shallow (d:Three a b elem))]
    [(d:Shallow (d:Three f s t)) (d:Deep (d:Two f s) d:empty (d:Two t elem))]
    [(d:Deep f m (d:Zero))       (d:Deep f m (d:One elem))]
    [(d:Deep f m (d:One a))      (d:Deep f m (d:Two a elem))]
    [(d:Deep f m (d:Two a b))    (d:Deep f m (d:Three a b elem))]
    [(d:Deep fi m (d:Three f s t)) 
     (d:Deep fi (d:enqueue (cons f s) m) (d:Two t elem))]))

;; Returns the front deque element, ie from the left
;(: head : (All (A) ((Deque A) -> A)))
(define (d:head que)
  (match que    
    [(d:Shallow (d:Zero))  (error 'head "given deque is empty")]
    [(d:Shallow (d:One f))        f]
    [(d:Shallow (d:Two f s))      f]
    [(d:Shallow (d:Three f s t))  f]
    [(d:Deep (d:Zero) m r) (error 'head "given deque is empty")]
    [(d:Deep (d:One f) m r)       f]
    [(d:Deep (d:Two f s) m r)     f]
    [(d:Deep (d:Three f s t) m r) f]))

;; Returns the last, ie rear/right, deque element
;(: last : (All (A) ((Deque A) -> A)))
(define (d:last que)
  (match que    
    [(d:Shallow (d:Zero))  (error 'last "given deque is empty")]
    [(d:Shallow (d:One f))         f]
    [(d:Shallow (d:Two f s))       s]
    [(d:Shallow (d:Three f s t))   t]
    [(d:Deep f m  (d:Zero)) (error 'last "given deque is empty")]
    [(d:Deep fi m (d:One f))       f]
    [(d:Deep fi m (d:Two f s))     s]
    [(d:Deep fi m (d:Three f s t)) t]))

;; drops the front, ie left, deque element
;(: tail : (All (A) ((Deque A) -> (Deque A))))
(define (d:tail que)
  (match que    
    [(d:Shallow (d:Zero))         (error 'tail "given deque is empty")]
    [(d:Shallow (d:One _))        (d:Shallow (d:Zero))]
    [(d:Shallow (d:Two _ s))      (d:Shallow (d:One s))]
    [(d:Shallow (d:Three _ s t))  (d:Shallow (d:Two s t))]
    [(d:Deep (d:Zero) m r)        (error 'tail "given deque is empty")]
    [(d:Deep (d:Three _ s t) m r) (d:Deep (d:Two s t) m r)]
    [(d:Deep (d:Two _ s) m r)     (d:Deep (d:One s) m r)]
    [(d:Deep (d:One _) mid r) 
     (if (d:empty? mid)
         (d:Shallow r)
         (let* ([p (d:head mid)]
                [fst (car p)]
                [snd (cdr p)])
           (d:Deep (d:Two fst snd) (d:tail mid) r)))]))

;; Returns a deque without rear, ie last/right, element
;(: init : (All (A) ((Deque A) -> (Deque A))))
(define (d:init que)
  (match que
    [(d:Shallow (d:Zero))           (error 'init "given deque is empty")]
    [(d:Shallow (d:One _))          (d:Shallow (d:Zero))]
    [(d:Shallow (d:Two f _))        (d:Shallow (d:One f))]
    [(d:Shallow (d:Three f s _))    (d:Shallow (d:Two f s))]
    [(d:Deep f m   (d:Zero))        (error 'init "given deque is empty")]
    [(d:Deep fi m  (d:Three f s _)) (d:Deep fi m (d:Two f s))]
    [(d:Deep fi m  (d:Two f _))     (d:Deep fi m (d:One f))]
    [(d:Deep f mid (d:One _))
     (if (d:empty? mid)
         (d:Shallow f)
         (let* ([p (d:last mid)]
                [fst (car p)]
                [snd (cdr p)])
           (d:Deep f (d:init mid) (d:Two fst snd))))]))


;; Similar to build-list
;(: build-deque : (All (A) (Natural (Natural -> A) -> (Deque A))))
(define (d:build-deque size func)
  (let loop ([n size])
        (if (zero? n)
            d:empty
            (let ([nsub1 (sub1 n)])
              (d:enqueue (func nsub1) (loop nsub1))))))
(define (d:build-deque-front size func)
  (let loop ([n size])
        (if (zero? n)
            d:empty
            (let ([nsub1 (sub1 n)])
              (d:enqueue-front (func nsub1) (loop nsub1))))))





;; Okasaki's Simple Catenable Deque (figure 11.3), ie c-deque
;; supports O(log n) amortized append



;; d is regular deque (with any number of elements)
(struct Shallow (d))
;; front and rear are regular deques of >= 2 elements, ie too-small? = #f
;; mid is c-deque of regular deques of >= 2 elements
(struct Deep (f m r))
  

;; returns #t if d has 0 or 1 elements
(define (too-small? d) (or (d:empty? d) (d:empty? (d:tail d))))

;; add d1's elements to front of d2
;; d1 has 0 or 1 elements, ie (too-small? d1) = #t
(define (dappendL d1 d2)
  (if (d:empty? d1) d2
      (d:enqueue-front (d:head d1) d2)))
;; add d2's elements to rear of d1
;; d2 has 0 or 1 elements, ie (too-small? d2) = #t
(define (dappendR d1 d2)
  (if (d:empty? d2) d1
      (d:enqueue (d:head d2) d1)))

(define empty (Shallow d:empty))
(define (empty? cd) (and (Shallow? cd) (d:empty? (Shallow-d cd))))

;; front, ie left, c-deque operations
;; enqueue-front is Okasaki's cons
(define (enqueue-front x cd)
  (match cd
    [(Shallow d)  (Shallow (d:enqueue-front x d))]
    [(Deep f m r) (Deep (d:enqueue-front x f) m r)]))
(define (head cd)
  (match cd
    [(Shallow d)  (d:head d)]
    [(Deep f m r) (d:head f)]))
(define (tail cd)
  (match cd
    [(Shallow d) (Shallow (d:tail d))]
    [(Deep f m r)
     (let ([tailf (d:tail f)])
       (cond [(not (too-small? tailf)) (Deep tailf m r)]
             [(empty? m) (Shallow (dappendL tailf r))]
             [else (Deep (dappendL tailf (head m)) (tail m) r)]))]))

;; rear, ie right, c-deque operations
;; enqueue is Okasaki's snoc
(define (enqueue x cd)
  (match cd
    [(Shallow d)  (Shallow (d:enqueue x d))]
    [(Deep f m r) (Deep f m (d:enqueue x r))]))
(define (last cd)
  (match cd
    [(Shallow d)  (d:last d)]
    [(Deep f m r) (d:last r)]))
(define (init cd)
  (match cd
    [(Shallow d) (Shallow (d:init d))]
    [(Deep f m r)
     (let ([initr (d:init r)])
       (cond [(not (too-small? initr)) (Deep f m initr)]
             [(empty? m) (Shallow (dappendR f initr))]
             [else (Deep f (init m) (dappendR (last m) initr))]))]))

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

#;(define (append cd1 cd2)
  (match* (cd1 cd2)
    [((Shallow d1) (Shallow d2))
     (cond [(too-small? d1) (Shallow (dappendL d1 d2))]
           [(too-small? d2) (Shallow (dappendR d1 d2))]
           [else (Deep d1 empty d2)])]
    [((Shallow d) (Deep f m r))
     (if (too-small? d)
         (Deep (dappendL d f) m r)
         (Deep d (enqueue-front f m) r))]
    [((Deep f m r) (Shallow d))
     (if (too-small? d)
         (Deep f m (dappendR r d))
         (Deep f (enqueue r m) d))]
    [((Deep f1 m1 r1) (Deep f2 m2 r2))
     (Deep (f1 (append (enqueue r1 m1) (enqueue-front f2 m2)) r2))]))

(define (append cd1 cd2)
  (match cd1
    [(Shallow d)
     (match cd2
       [(Shallow d2)
        (cond [(too-small? d) (Shallow (dappendL d d2))]
              [(too-small? d2) (Shallow (dappendR d d2))]
              [else (Deep d empty d2)])]
       [(Deep f m r)
        (if (too-small? d)
            (Deep (dappendL d f) m r)
            (Deep d (enqueue-front f m) r))])]
    [(Deep f m r)
     (match cd2
       [(Shallow d)
        (if (too-small? d)
            (Deep f m (dappendR r d))
            (Deep f (enqueue r m) d))]
       [(Deep f2 m2 r2)
        (Deep f (append (enqueue r m) (enqueue-front f2 m2)) r2)])]))

(define (build-deque-with-append size f)
  (let loop ([n size] [f f])
    (if (<= n 2)
        (enqueue (f (sub1 n)) (enqueue (f (- n 2)) empty))
        (let ([n/2 (/ n 2)])
          (append (loop n/2 (λ (x) (f x)))
                  (loop n/2 (λ (x) (+ n/2 (f x)))))))))

;; sums the x front, ie left, deque elements
(define (sum-front-x x q)
  (let loop ([n 0] [d q])
    (if (= n x) 0
        (+ (head d) (loop (add1 n) (tail d))))))
;; sums the x rear, ie right, deque elements
(define (sum-rear-x x q)
  (let loop ([n 0] [d q])
    (if (= n x) 0
        (+ (last d) (loop (add1 n) (init d))))))
;(define size 1030)
;(define d (build-deque size (λ (x) x)))
;(define dfront (build-deque-front size (λ (x) x)))
;(sum-front-x 100 d)
;(sum-rear-x 20 dfront)

;(define size/2 size)
;(define d1 (build-deque size/2 (λ (x) x)))
;(define d2 (build-deque size/2 (λ (x) (+ x size/2))))
;(define dapp (append d1 d2))
;(define d1front (build-deque-front size/2 (λ (x) x)))
;(define d2front (build-deque-front size/2 (λ (x) (+ x size/2))))
;(define dappfront (append d2front d1front))
#;(sum-front-x 10 (append (build-deque-with-append 512 (λ (x) x))
                        (build-deque-with-append 512 (λ (x) (+ x 512)))))
(sum-front-x 2 (build-deque-with-append 1024 add1))

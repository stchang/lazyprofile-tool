#lang racket

;; TODO: [o] = open, [x] = fixed
;; [o] 2013-05-09 match* broken; see deque-catenable-profiled.rkt
;;                getting mysterious "uncaught exception: #<void>" error

(require (for-syntax racket/syntax ; for format-id
                     racket/struct-info
                     syntax/parse))

(require (prefix-in is: data/integer-set))

(require 2htdp/image)
(require 2htdp/universe)

(require "utils.rkt")

(define SHOW-SUBTRACT-AND-ERASE-DEBUG-INFO #f)
(define SHOW-UNUSED-ONLY #t)

;; every value that we want to track is wrapped with a profiled struct
;; and is assigned a unique vnum
(struct profiled (v vnum))
(struct syntx (stx ln col)) ; struct for storing syntax info at runtime

;; ----------------------------------------------------------------------------
;; ---------- ANUMS ----------
;; ----------------------------------------------------------------------------
(define-for-syntax arg-num 0)
(define-for-syntax (get-new-arg-num)
  (begin0 arg-num (set! arg-num (add1 arg-num))))

;; ----------------------------------------------------------------------------
;; ---------- anum-stx-info
;; maps arg-num to syntax info (stored in syntx struct)
;; used to transfer syntax info from compile time to run time
;; (is there a more elegant way to do this?)
(define anum-stx-info (make-hash))
(define (anum-exists? anum)
  (if (hash-ref anum-stx-info anum #f)
      #t
      #f))
(define total-anums 0)
(define total-uses 0)
(define (set!-total-anums) 
  ;; for total-anums, cant just do hash-count anum-stx-info because args that 
  ;; are never evaled, ie, in a delay, and wont have an entry in anum-stx-info
  (set! total-anums (add1 (apply max 0 (hash-keys anum-stx-info))))
  (set! all-existing-anums (hash-keys anum-stx-info))
  (set! total-uses (apply + (hash-values vnum-usage-total))))
(define (get-total-anums) total-anums)
(define all-existing-anums null)
(define (anum->stx anum)  (syntx-stx (hash-ref anum-stx-info anum)))
#;(define (vnum->stx vnum) 
  (if (eq? vnum 'top) 'top (anum->stx (vnum->anum vnum))))
(define (anum->str anum)
  (match-define (syntx stx ln col) (hash-ref anum-stx-info anum))
  (format "exp#~a ~a [ln ~a, col ~a]" anum stx ln col))
#;(define SHOW-VNUM-VAL #f)
#;(define (vnum->str vnum) 
  (if SHOW-VNUM-VAL
      (format "*v~a* ~a => ~a" vnum (vnum->stx vnum) (vnum->v vnum))
      (format "*v~a* ~a" vnum (vnum->stx vnum))))
#;(define (print-vnum vnum) (printf "~a" (vnum->str vnum)))



;; ----------------------------------------------------------------------------
;; ---------- VNUMS ----------
;; ----------------------------------------------------------------------------
(define val-num 0)
(define (get-new-val-num) 
  (begin0 val-num (set! val-num (add1 val-num))))
(define (print-total-vnums) 
  (newline)
  (printf "total anums:\t\t~a\n"         (get-total-anums))
  (printf "total vnums created:\t~a\n" val-num #;(- val-num (set-count erased-vnums)))
  (printf "initial unused vnums:\t~a\n"   
          (- val-num (count (λ (x) (not (zero? x))) (hash-keys vnum-usage-total)) 1)
          #;(apply + (hash-values vnum-usage-total)))
  (newline))

;; maps vnum to the actual value represented by the vnum
;(define vnum-to-v (make-hash '((top . top))))
;(define (vnum->v vnum) (hash-ref vnum-to-v vnum))

;; ----------------------------------------------------------------------------
;; ---------- anum-to-vnums
;; maps arg-nums to set of vnums created by that app-num
(define anum-to-vnums (make-hash))
;; maps val-num to arg-num of creating expression
;(define vnum-to-anum (make-hash))
;(define (vnum->anum vnum) (hash-ref vnum-to-anum vnum))


;; ----------------------------------------------------------------------------
;; vnum-usage-per-vnum
;; maps vnums to hash of vnums to counts
;; the value hash is vnum usage tally while evaluating the key vnum
;; (includes all sub vnums, so adding usage of a vnum and a sub-vnum of that
;; vnum will double count 
;; (in other words, in mark-strict-pos, I add1 for all vnum \in vnum-ctxt,
;;  instead of just (car vnum-ctxt))
;; 2013-01-26: no longer includes sub vnums
;; (so in mark-strict-pos, add1 only for (car vnum-ctxt)
(define vnum-usage-per-vnum (make-hash))
#;(define (print-vnum-usage-per-vnum)
  (printf "------------------------ vnum-usage-per-vnum ----------------------------------\n")
  (for ([(vnum vnum-usage) (in-hash vnum-usage-per-vnum)])
    (printf "while eval ~a:\n" (vnum->str vnum))
    (for ([(vnum count) (in-hash vnum-usage)])
      (printf "  used ~a: ~a times\n" (vnum->str vnum) count)))
  (newline))
(define (add1!-to-per-vnum-usage vnum used-vnum)
  (hash-update! vnum-usage-per-vnum
                vnum
                (λ (h) (hash-update! h used-vnum add1 0) h)
                (make-hash)))


;; ----------------------------------------------------------------------------
;; ---------- vnum-usage-total
;; total usage counts for each vnum
;; maps vnums to counts
(define vnum-usage-total (make-hash))

;; some vnum predicates (and other simple functions) --------------------------
;; "used" = count > 0
;; uses "of" a vnum (ie, check vnum-usage-total[vnum])
(define (vnum-uses vnum) (hash-ref vnum-usage-total vnum 0))
;; uses "by" a vnum, ie sum uses in vnum-usage-per-vnum[vnum]
(define (get-uses-by-vnum vnum)
  (apply + (hash-values (hash-ref vnum-usage-per-vnum vnum (make-hash)))))
(define (vnum-used? vnum) (> (vnum-uses vnum) 0))
(define (vnum-unused? vnum) (zero? (vnum-uses vnum)))
(define (vnum-exists? vnum) (not (set-member? erased-vnums vnum)))
(define (vnum-unused-and-exists? vnum) (and (vnum-unused? vnum) (vnum-exists? vnum)))

#;(define (print-vnum-usage-total)
  (printf "----------------------- vnum-total-usage: ------------------------------------\n")
  (for ([(vnum count) (in-hash vnum-usage-total)])
    (printf "~a used ~a times\n" (vnum->str vnum) count)))
(define (add1!-to-usage vnum)
  (hash-update! vnum-usage-total vnum add1 0))
(define (add!-to-usage vnum add-count)
  (hash-update! vnum-usage-total vnum (λ (count) (+ count add-count)) 0))
;; subtract from vnum usage total
(define (sub!-from-usage vnum sub-count) 
  #;(when SHOW-SUBTRACT-AND-ERASE-DEBUG-INFO
    (printf "  subtracting ~a use(s) of ~a\n" sub-count (vnum->str vnum)))
  ; want error if vnum \notin vnum-usage-total
  (hash-update! vnum-usage-total vnum 
                (λ (count) 
                  (when (< count sub-count)
                    (printf "########################################\n")
                    (printf "SHOULDN'T GET HERE: negative uses after subtracting\n")
                    (printf "########################################\n"))
                  (- count sub-count))))
;; subtract all uses caused by evaluating vnum
(define (subtract!-vnum-usages vnum)
  #;(when SHOW-SUBTRACT-AND-ERASE-DEBUG-INFO
    (printf "!subtracting!: vnums used by unused vnum ~a:\n" (vnum->str vnum)))
  ;; this when is unneeded because subtracting usages and erasing subvnums should be indepedent
  ;; otherwise, you might erase a vnum before subtracting its uses
  (when #t #;(vnum-exists? vnum) ; avoid duplicate subtractions
    (for ([(vnum-to-sub count-to-sub)
           (in-hash (hash-ref vnum-usage-per-vnum vnum (make-hash)))])
    ;  (hash-update! anum-enablers (vnum->anum vnum-to-sub) (λ (s) (set-add s (vnum->anum vnum))) (set))
      (sub!-from-usage vnum-to-sub count-to-sub))))


;; ----------------------------------------------------------------------------
;; ---------- erased-vnums
(define erased-vnums (set))
;; erase a single vnum
(define (add!-erased-vnum vnum) (set! erased-vnums (set-add erased-vnums vnum)))
;; recursively erase a vnum and all descendents and subtract their usages
(define (erase!-vnum vnum)
  (when (vnum-exists? vnum)
    #;(when SHOW-SUBTRACT-AND-ERASE-DEBUG-INFO
      (printf "!erasing!: vnum ~a\n" (vnum->str vnum)))
    (add!-erased-vnum vnum)
    (subtract!-vnum-usages vnum)
    (for ([sub-vnum (in-range (add1 vnum) (hash-ref sub-vnums vnum (add1 vnum)))
                    #;(get-vnums-created-by-vnum vnum)]
          #:when (vnum-exists? sub-vnum))
      (erase!-vnum sub-vnum))
    ))

;; ----------------------------------------------------------------------------
;; ---------- vnums-evaled-hash
;; maps vnum to set of vnums
;; the value set of vnums are evaluated while evaluating the key vnum
#;(define vnums-evaled-hash (make-hash))
#;(define (print-vnums-evaled-hash)
  (printf "----------------------- vnum creation hierarchy: ------------------------------\n")
  (for ([(vnum vnums-created) (in-hash vnums-evaled-hash)])
    (printf "while eval ~a, created:\n" (vnum->str vnum))
    (for ([sub-vnum (in-set vnums-created)])
      (printf "  ~a\n" (vnum->str sub-vnum)))))
#;(define (get-vnums-created-by-vnum vnum)
  (set-filter vnum-exists? (hash-ref vnums-evaled-hash vnum (set))))

;; maps vnum to current val-num when eval of the key vnum is done
;; range of key to value vnums (non-inclusive) is the sub vnums
(define sub-vnums (make-hash))


;; ----------------------------------------------------------------------------
;; ---------- vnum-ctxt
;; list of vnums that are being evaluated, front of list is innermost vnum
;(define vnum-ctxt (make-parameter null))
(define vnum-ctxt '(top))
(define (set!-vnum-ctxt vnums) (set! vnum-ctxt vnums))


;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------
;; ---------- utility annotating functions
;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

;; instruments expression in strict position
(define-for-syntax (mark-strict-pos vstx)
  (with-syntax ([v vstx])
    (syntax/loc vstx
      (match v
        [(profiled val vnums)
         (for ([used-vnum vnums])
           (add1!-to-usage used-vnum)
           #;(for ([ctxt-vnum (vnum-ctxt)])
               (add1!-to-per-vnum-usage ctxt-vnum used-vnum))
;           (unless (null? vnum-ctxt)
             (add1!-to-per-vnum-usage (car vnum-ctxt) used-vnum))
         val]
        [val val]))))
  
(define-for-syntax defined-prims null)
;; instruments expression in argument position
(define-for-syntax (mark-arg-pos astx)
  (define ln (syntax-line astx))
  (define col (syntax-column astx))
  ;; only profile applications, skip literals, identifiers, and lambdas
  (define (annotate? stx)
    (and (pair? (syntax-e stx))
         (let* ([es (syntax->list stx)]
                [f (car es)]
                [args (cdr es)]
                [prims 
                 (syntax->list 
                  #'(car cdr null? zero? not = < > <= >= - + * / add1 sub1 cons))])
;                  #'(car cdr null? zero? not = < > <= >= - * / add1 sub1 cons))])
           (or (not (identifier? f))
           (and (identifier? f)
           (not (or (free-identifier=? #'λ f)
                    (free-identifier=? #'lambda f)
                    (free-identifier=? #'delay f)
                    (free-identifier=? #'lazy f)
                    (free-identifier=? #'define f)
                    ;; temporarily just compare datums
                    ;; free identifier=? not working because I'm comparing
                    ;;   profiled version to non-profiled
                    ;;   ie profiled-+ vs +
                    ;; TODO: fixme
                    (and (or (ormap (λ (x) (free-identifier=? x f) #;(eq? (syntax->datum x) (syntax->datum f)))
                                    prims)
                             (ormap (λ (x) (eq? x (syntax->datum f))) defined-prims))
                         (andmap (λ (x) (not (annotate? x))) args))
                    )))))))
  (if (annotate? astx)
      (let ([new-anum (get-new-arg-num)]) ; compile time anum
        (with-syntax ([arg astx])
          (quasisyntax/loc astx
            (let* ([anum #,new-anum] ; transfer compile time value to run time
                   [vnum (get-new-val-num)]
                   #;[v (parameterize ([vnum-ctxt (cons vnum (vnum-ctxt))])
                        arg)]  ; do the eval, setting the ctxt
;                   [set-vnum-ctxt-tmp (set!-vnum-ctxt (cons vnum vnum-ctxt))] ; push ctxt
;                   [v arg] ; do the eval
;                   [unset-vnum-ctxt-tmp (set!-vnum-ctxt (cdr vnum-ctxt))]
                   )
              ;; ) store anum stx info
;              (unless (hash-has-key? anum-stx-info anum) ; prevent redundant updates
                (hash-set! anum-stx-info 
                           anum 
                           (syntx (syntax->datum #'arg) #,ln #,col))
              ;; ) store vnum value
;              (hash-set! vnum-to-v vnum (unprofile v))
              ;; ) associate vnum with anum
              (hash-update! anum-to-vnums
                            anum
                            (λ (s) (set-add s vnum))
                            (set))
              ;; ) store vnum to anum reverse lookup
;              (hash-set! vnum-to-anum vnum anum)
              ;; ---- store context related info
;              (define ctxt-vnums (vnum-ctxt))
              ;; add vnum as created while evaluating each ctxt-vnum in ctxt-vnums
;              (printf "vnum range of ~a: ~a to ~a\n" (anum->stx anum) vnum val-num)
              #;(for ([ctxt-vnum vnum-ctxt])
                (hash-update! vnums-evaled-hash
                              ctxt-vnum
                              (λ (s) (set-add s vnum))
                              (set)))
              (let ([set-vnum-ctxt-tmp (set!-vnum-ctxt (cons vnum vnum-ctxt))] ; push ctxt
                    [v arg] ; do the eval
                    [unset-vnum-ctxt-tmp (set!-vnum-ctxt (cdr vnum-ctxt))])
                (hash-set! sub-vnums vnum val-num)
              ;; need to accumulate anums and vnums, for when annotated vals
              ;; pass through tail positions, see explanation in 
              ;; lazy-profile-testing.rkt
              ;; - keep anums and vnums in two separate lists, but each anum
              ;;   is associated with each corresponding vnum
              (match v
                [(profiled val vnums)
                 (profiled val (cons vnum vnums))]
                ;; only unannotated val, so create new profiled struct
                [_ (profiled v (list vnum))]))))))
      astx))

;; mark-arg-pos if not a define
(define-for-syntax (mark-non-def-arg-pos stx)
  (syntax-case stx ()
    [(d x e)
     (free-identifier=? #'d #'profiled-define)
     stx]
    [_ (mark-arg-pos stx)]))



;; ----------------------------------------------------------------------------
;; Create profiled- versions of functions

;; given a function identifier, creates a version with "profiled-" prefix,
;; but provides the prefixed version renamed as the original function, 
;; ie, profiled-f is provided as just f
(define-syntax (make-profiled-fn stx)
  (syntax-case stx ()
    [(_ f)
     (let ([new-id (format-id stx "profiled-~a" #'f)])
       #`(begin
           (provide (rename-out [#,new-id f]))
           (define (#,new-id . args)
             (define new-args
               (map 
                ; only need mark-strict pos, mark-arg-pos already applied
                ; by app (or let)
                (λ (arg) #,(mark-strict-pos #'arg))
                args))
             (apply f new-args))))]))

;; similar to make-profiled-fn, but requires deep unprofiling of args
#;(define-syntax (make-profiled-fn-that-needs-unprofiling stx)
  (syntax-case stx ()
    [(_ f)
     (let ([new-id (format-id stx "profiled-~a" #'f)])
       #`(begin
           (provide (rename-out [#,new-id f]))
           (define (#,new-id . args)
             (define new-args
               (map 
                ; only need mark-strict pos, mark-arg-pos already applied
                ; by app (or let)
                (λ (arg) #,(mark-strict-pos #'arg))
                args))
             (define unprofiled-args (map unprofile-list-structure new-args))
             (apply f unprofiled-args))))]))
;; ************************** List of Profiled Macros *************************
(define-for-syntax profiled-forms
  (syntax->list 
   #'(#%app #%module-begin let let* if cond and or struct define match match*
      values for/fold for/and for/list for*/list printf set!)))
;; ****************************************************************************

#;(define-for-syntax more-profiled-fns #'(list?))
(define-for-syntax other-profiled-fns 
  #'(first rest second third cadr caadr length sequence-length 
           append findf equal? reverse string-join list->string new send))

(define-syntax (make-profiled-fns stx)
  (syntax-case stx ()
    [(_ f ...)
     (with-syntax ([(other-f ...) other-profiled-fns]
                   [(profiled-other-f ...) 
                    (map (λ (id) (format-id id "profiled-~a" id))
                         (syntax->list other-profiled-fns))])
       #`(begin
           (provide (except-out (all-from-out racket) 
                                #,@profiled-forms
;                                #,@more-profiled-fns
                                #,@other-profiled-fns
                                f ...))
           (make-profiled-fn f) ...
           (provide (rename-out [profiled-other-f other-f] ...))
           ))]))
(define-syntax (make-2htdp-profiled-fns stx)
  (syntax-case stx ()
    [(_ f ...)
     #`(begin
         (provide (except-out (combine-out (all-from-out 2htdp/image) 
                                           (all-from-out 2htdp/universe))
                              ;big-bang
                              f ...))
         (make-profiled-fn f) ...
         )]))
#;(define-syntax (make-profiled-fns-that-need-unprofiling stx)
  (syntax-case stx ()
    [(_)
     (with-syntax ([(f ...) more-profiled-fns])
       #`(begin
           (make-profiled-fn-that-needs-unprofiling f)
           ...))]))

;; ************************ List of Profiled Functions ************************
(make-profiled-fns car cdr null? + - * / >= <= < > = not
                   add1 sub1 even? odd? zero? modulo quotient
                   eq? eqv? empty?
                   cons? pair? force
                   random arithmetic-shift
                   string=? string-ref substring char=? string string-append
                   max min argmin argmax
                   error list-ref member map apply
                   in-range open-input-string read file-position port->string
                   char-alphabetic? char-numeric? char->integer integer->char char-whitespace?)
(make-2htdp-profiled-fns image-width image-height rotate regular-polygon text
                         overlay overlay/offset place-image key=? empty-scene)
;(make-profiled-fns-that-need-unprofiling) ; see more-profiled-fns above
;; ****************************************************************************

(define-syntax (profiled-equal? stx)
  (syntax-case stx ()
    [(_ a b)
     #`(let loop ([xx a] [yy b])
         (define x #,(mark-strict-pos #'xx))
         (define y #,(mark-strict-pos #'yy))
         (if (pair? x)
             (if (pair? y)
                 (andmap loop x y)
                 #f)
             (equal? x y)))]))
(define-syntax (profiled-first stx)
  (syntax-case stx ()
    [(_ lst) #`(car #,(mark-strict-pos #'lst))]))
(define-syntax (profiled-rest stx)
  (syntax-case stx ()
    [(_ lst) #`(cdr #,(mark-strict-pos #'lst))]))
(define-syntax (profiled-second stx)
  (syntax-case stx ()
    [(_ lst) #`(car #,(mark-strict-pos #`(cdr #,(mark-strict-pos #'lst))))]
    [_ #`(λ (lst) #,(mark-strict-pos #`(car #,(mark-strict-pos #`(cdr #,(mark-strict-pos #'lst))))))]))
(define-syntax (profiled-third stx)
  (syntax-case stx ()
    [(_ lst) 
     #`(car #,(mark-strict-pos 
               #`(cdr #,(mark-strict-pos 
                         #`(cdr #,(mark-strict-pos #'lst))))))]))
(define-syntax-rule (profiled-cadr lst) (profiled-second lst))
(define-syntax-rule (profiled-caadr lst) (profiled-third lst))

;; ----------------------------------------------------------------------------
;; Create profiled- versions of macros

(provide (rename-out [app #%app]
                     [mbegin #%module-begin]
                     [profiled-let let] [profiled-let* let*]
                     [profiled-if if]
                     [profiled-cond cond]
                     [profiled-and and] [profiled-or or]
                     [profiled-match match]
                     [profiled-match* match*]
                     [profiled-struct struct]
                     [profiled-define define]
                     [profiled-set! set!]
;                     [profiled-append append]
;                     [profiled-findf findf]
;                     [profiled-lambda lambda]
;                     [profiled-lambda λ]
                     [profiled-values values]
                     [profiled-for/fold for/fold]
                     [profiled-for/and for/and]
                     [profiled-for/list for/list]
                     [profiled-for*/list for*/list]
                     [profiled-printf printf]
    ;                 [profiled-define-values define-values]
;                     [profiled-big-bang big-bang]
                     ))

;(define-syntax (profiled-lambda stx)
;  (syntax-case stx ()
;    [(_ (args ...) body ...)
;     (with-syntax ([(new-body ...) 
;                    (map mark-arg-pos (syntax->list #'(body ...)))])
;       #'(lambda (args ...) new-body ...))]))

(define-syntax (profiled-printf stx)
  (syntax-case stx ()
    [(_ str arg ...)
     (with-syntax ([(strict-arg ...) (map mark-strict-pos (syntax->list #'(arg ...)))])
       #'(begin
           (printf "---raw value:\n")
           (printf str arg ...)
           (printf "---unprofiled:\n")
           (printf str strict-arg ...)))]))

(provide define/prim)
(define-syntax (define/prim stx)
  (syntax-case stx ()
    [(_ (name args ...) body ...)
     (set! defined-prims (cons (syntax->datum #'name) defined-prims))
     #'(define (name args ...) body ...)]))

(define-syntax (profiled-match stx)
  (syntax-case stx ()
    [(_ test clause ...) #`(match #,(mark-arg-pos #'test) clause ...)]))
(define-syntax (profiled-match* stx)
  (syntax-case stx ()
    [(_ (test ...) clause ...) 
     (with-syntax ([(new-test ...)
                    (map mark-arg-pos (syntax->list #'(test ...)))])
       #`(match* (new-test ...) clause ...))]))

(define-syntax (profiled-define stx)
  (syntax-case stx ()
    [(_ (name args ...) body ...) 
     #;(with-syntax ([(new-body ...) 
                    (map mark-arg-pos (syntax->list #'(body ...)))])
         #'(define (name args ...) new-body ...))
     #'(define (name args ...) body ...)]
    [(_ name e) #`(define name #,(mark-arg-pos #'e))]))

#;(define-syntax (profiled-define-values stx)
  [(syntax-case stx ()
     [(_ (x ...) body ...) #'(define-values (x ...) body ...)])])

(define-syntax (profiled-set! stx)
  (syntax-case stx ()
    [(_ name e) #`(set! name #,(mark-arg-pos #'e))]))


(define-syntax (profiled-struct stx)
  (define (mk-?-id id) (format-id id "~a?" id))
  (define (mk-new-id id) (format-id id "new-~a" id))
  (syntax-case stx ()
    [(_ name (field ...) option ...) 
     (with-syntax* 
      ([pred? (mk-?-id #'name)]
       [(field-f ...)
        (map (λ (f) (format-id stx "~a-~a" #'name f))
             (syntax->list #'(field ...)))]
       [tmp-name (format-id #'here "~a" (generate-temporary #'name))]
       [tmp-pred? (mk-?-id #'tmp-name)]
       [(tmp-field-f ...)
        (map (λ (f) (format-id #'here "~a-~a" #'tmp-name f))
             (syntax->list #'(field ...)))]
       [new-struct-info (generate-temporary
                         (format-id #'here "~a-struct-info" #'new-name))]
       [(falses ...) (map (λ _ #f) (syntax->list #'(field ...)))])
      #`(begin
          (struct tmp-name (field ...) option ...)
          (define (pred? x) (tmp-pred? #,(mark-strict-pos #'x)))
          (define (field-f x) (tmp-field-f #,(mark-strict-pos #'x)))
          ...
          (define-syntax new-struct-info 
            (make-struct-info 
             (λ () (list #'tmp-name
                         #'tmp-name
                         #'pred?
                         (reverse (list (syntax field-f) ...))
                         (list falses ...) #t))))
          (...
           (define-match-expander name
             (syntax-rules () [(_ e ...) (new-struct-info e ...)])
             (λ (stx)
               (syntax-case stx ()
                 [(_ e ...)
                  (with-syntax ([(new-e ...) 
                                 (map #,mark-arg-pos (syntax->list #'(e ...)))])
                    #'(tmp-name new-e ...))]
                 [_ #'tmp-name]
                 ))))))]))

(define-syntax (profiled-let stx)
  (syntax-case stx ()
    [(_ name ([x e] ...) body ...)
     (with-syntax ([(new-e ...) (map mark-arg-pos (syntax->list #'(e ...)))]
                   #;[(new-body ...) (map mark-arg-pos (syntax->list #'(body ...)))])
       #'(let name ([x new-e] ...) body ...))]
    [(_ ([x e] ...) body ...)
     (with-syntax ([(new-e ...) (map mark-arg-pos (syntax->list #'(e ...)))]
                   #;[(new-body ...) (map mark-arg-pos (syntax->list #'(body ...)))])
       #'(let ([x new-e] ...) body ...))]))
(define-syntax (profiled-let* stx)
  (syntax-case stx ()
    [(_ ([x e] ...) body ...)
     (with-syntax ([(new-e ...) (map mark-arg-pos (syntax->list #'(e ...)))]
                   #;[(new-body ...) (map mark-arg-pos (syntax->list #'(body ...)))])
       #'(let* ([x new-e] ...) body ...))]))

(define-syntax (app stx)
  (syntax-case stx ()
    [(_ f arg ...)
     (with-syntax 
         ([new-f (mark-strict-pos #'f)]
          [(new-arg ...) (map mark-arg-pos (syntax->list #'(arg ...)))])
       #`(let ([tick-vnum (get-new-val-num)]) (#%app new-f new-arg ...)))]))

(define-syntax (profiled-if stx)
  (syntax-case stx ()
    [(_ e1 e2 e3)
     #`(if #,(mark-strict-pos #'e1) e2 e3)]))
(define-syntax (profiled-cond stx)
  (syntax-case stx (else)
    [(_ [tst body ...] ... [else elsebody ...])
     (with-syntax ([(strict-tst ...) (map mark-strict-pos (syntax->list #'(tst ...)))])
       #`(cond [strict-tst body ...] ... [else elsebody ...]))]
    [(_ [tst body ...] ...)
     (with-syntax ([(strict-tst ...) (map mark-strict-pos (syntax->list #'(tst ...)))])
       #`(cond [strict-tst body ...] ...))]))

(define-syntax (profiled-and stx)
  (syntax-case stx ()
    [(_ e) (mark-strict-pos #'e)]
    [(_ e0 e ...)
;     #`(profiled-if e0 (profiled-and e ...) #f)]))
     #`(and #,(mark-strict-pos #'e0) (profiled-and e ...))]))

;; TODO 2013-01-20: I think I can replace the let and if with just a
;;                  (non-profiled) or
(define-syntax (profiled-or stx)
  (syntax-case stx ()
    [(_ e) (mark-strict-pos #'e)]
    [(_ e0 e ...)
     #`(or #,(mark-strict-pos #'e0) (profiled-or e ...))]))
;     #`(let ([res #,(mark-strict-pos #'e0)])
;         (if res
;             res
;             (profiled-or e ...)))]))
                    
(define-syntax (profiled-values stx)
  (syntax-case stx ()
    [(_ e ...)
     (with-syntax ([(arg ...) (map mark-arg-pos (syntax->list #'(e ...)))])
       #'(values arg ...))]))

(define-syntax-rule (my-first lst)
  (cond [(stream? lst) (stream-first lst)]
        [(pair? lst) (car lst)]
        [else (error 'my-first)]))
(define-syntax-rule (my-rest lst)
  (cond [(stream? lst) (stream-rest lst)]
        [(pair? lst) (cdr lst)]
        [else (error 'my-rest)]))
(define-syntax-rule (my-null? lst)
  (cond [(stream? lst) (stream-empty? lst)]
        [else (not (pair? lst))]))

(define-syntax (profiled-for/fold stx)
  (syntax-case stx ()
    [(_ ([accx acc] ...) ([x seq] ...) body ...)
     ;; fine to mark seq with strict pos because need to test and extract on every loop
     (with-syntax* ([(ann-acc ...) (map mark-arg-pos (syntax->list #'(acc ...)))]
                    #;[(ann-seq ...) (map mark-arg-pos (syntax->list #'(seq ...)))]
                    [(res ...) (generate-temporaries #'(acc ...))]
                    [(seq-tmp ...) (generate-temporaries #'(seq ...))]
                    [(strict-seq ...) (map mark-strict-pos (syntax->list #'(seq-tmp ...)))])
     #`(let loop ([accx ann-acc] ... [seq-tmp seq] ...);[seq-tmp ann-seq] ...)
         (cond [(or (my-null? strict-seq) ...)
                (values accx ...)]
               [else
                (define x (my-first strict-seq)) ...
                (define-values (res ...) (let () body ...))
                (loop res ... (my-rest strict-seq) ...)])))]))
(define-syntax (profiled-for/and stx)
  (syntax-case stx ()
;    #;[(_ ([x seq] ... when tst) body ...)
;     #;(free-identifier=? #'#:when #'when) (eq? (syntax->datum #'when) '#:when)
;     (with-syntax* ([(seq-tmpx ...) (generate-temporaries #'(seq ...))]
;                    #;[(ann-seq ...) (map mark-arg-pos (syntax->list #'(seq ...)))]
;                    [(strict-seq ...) (map mark-strict-pos (syntax->list #'(seq-tmpx ...)))])
;       #'(let loop ([seq-tmpx seq] ...)
;           (or (my-null? strict-seq) ...
;               (and
;                (let ([x (my-first strict-seq)] ...)
;                  (or (not tst)
;                      (begin body ...)))
;                (loop (my-rest strict-seq) ...)))))]
    [(_ ([x seq] ...) body ...)
     (with-syntax* ([(seq-tmpx ...) (generate-temporaries #'(seq ...))]
                    #;[(ann-seq ...) (map mark-arg-pos (syntax->list #'(seq ...)))]
                    [(strict-seq ...) (map mark-strict-pos (syntax->list #'(seq-tmpx ...)))])
       #'(let loop ([seq-tmpx seq] ...)
           (or (my-null? strict-seq) ...
               (and
                (let ([x (my-first strict-seq)] ...) body ...)
                (loop (my-rest strict-seq) ...)))))]
    ))
(define-syntax (profiled-for/list stx)
  (syntax-case stx ()
    [(_ ([x seq] ...) body ...)
     (with-syntax* ([(arg-body ...) (map mark-non-def-arg-pos (syntax->list #'(body ...)))]
                    [(seq-tmpx ...) (generate-temporaries #'(seq ...))]
                    #;[(ann-seq ...) (map mark-arg-pos (syntax->list #'(seq ...)))]
                    [(strict-seq ...) (map mark-strict-pos (syntax->list #'(seq-tmpx ...)))])
       #`(let loop ([seq-tmpx seq] ...)
           (if (or (my-null? strict-seq) ...)
               null
               (cons (let ([x (my-first strict-seq) #;(sequence-ref strict-seq 0)] ...) arg-body ...)
                     (loop (my-rest strict-seq) #;(sequence-tail strict-seq 1) ...)))))]))

(define-syntax (profiled-for*/list stx)
  (syntax-parse stx
    [(_ ([x seq] (~optional (~seq #:when tst) #:defaults ([tst #'#t]))) body ...)
;     (eq? (syntax->datum #'when) '#:when) ;(free-identifier=? #'#:when #'when)
     (with-syntax* ([(arg-body ...) (map mark-non-def-arg-pos (syntax->list #'(body ...)))]
                    [seq-tmpx (generate-temporary #'seq)]
                    #;[ann-seq (map mark-arg-pos (syntax->list #'seq))]
                    [strict-seq (mark-strict-pos #'seq-tmpx)])
       #`(let loop ([seq-tmpx seq])
           (if (my-null? strict-seq) #;(stream-empty? strict-seq)
               null
               (let* ([x (my-first strict-seq) #;(sequence-ref strict-seq 0)])
                 (cond [#,(mark-strict-pos #'tst)
                        (define (res) arg-body ...)
                        (cons (res)
                              (loop (my-rest strict-seq) #;(sequence-tail strict-seq 1)))]
                       [else (loop (my-rest strict-seq) #;(sequence-tail strict-seq 1))])))))]
;    #;[(_ ([x seq] ... when tst) body ...)
;     (eq? (syntax->datum #'when) '#:when)
;     (with-syntax* ([(arg-body ...) (map mark-non-def-arg-pos (syntax->list #'(body ...)))]
;                    [(seq-tmpx ...) (generate-temporaries #'(seq ...))]
;                    #;[(ann-seq ...) (map mark-arg-pos (syntax->list #'(seq ...)))]
;                    [(strict-seq ...) (map mark-strict-pos (syntax->list #'(seq-tmpx ...)))])
;       #`(let loop ([seq-tmpx seq] ...)
;           (if (or (my-null? strict-seq) #;(stream-empty? strict-seq) ...)
;               null
;               (let* ([x (my-first strict-seq) #;(sequence-ref strict-seq 0)] ...)
;                 (cond [#,(mark-strict-pos #'tst)
;                        (define (res) arg-body ...)
;                        (cons (res)
;                              (loop (my-rest strict-seq) #;(sequence-tail strict-seq 1) ...))]
;                       [else (loop (my-rest strict-seq) #;(sequence-tail strict-seq 1) ...)])))))]
;    [(_ ([x seq]) body ...) #'(profiled-for/list ([x seq]) body ...)]
    [(_ ([x seq] rst ...) body ...)
     #`((λ (lst) (foldr (λ (a acc) (append #,(mark-strict-pos #'a) acc)) null lst))
        (profiled-for/list ([x seq])
         (profiled-for*/list (rst ...) body ...)))]
    ))

(define-syntax (profiled-length stx)
  (syntax-case stx ()
    [(_ lst)
     #`(let loop ([x lst])
         (if (null? #,(mark-strict-pos #'x)) 0
             (add1 (loop (cdr #,(mark-strict-pos #'x))))))]))
(define-syntax (profiled-sequence-length stx)
  (syntax-case stx ()
    [(_ lst)
     #`(let loop ([x lst])
         (if (stream-empty? #,(mark-strict-pos #'x))
             (add1 (loop (sequence-tail #,(mark-strict-pos #'x) 1)))
             0))]))
(define-syntax (profiled-append stx)
  (syntax-case stx ()
    [(_ lst) #'lst]
    [(_ lst1 lst2 rest ...)
     #`(let loop ([x lst1] [y lst2])
         (if #,(mark-strict-pos #`(null? #,(mark-strict-pos #'x)))
             (profiled-append y rest ...)
             (cons (car #,(mark-strict-pos #'x)) (loop (cdr #,(mark-strict-pos #'x)) y))))]))
(define-syntax (profiled-findf stx)
  (syntax-case stx ()
    [(_ p? lst)
     #`(let loop ([x lst])
         (if (null? #,(mark-strict-pos #'x)) #f
             (if #,(mark-strict-pos #`(p? #,(mark-strict-pos #`(car #,(mark-strict-pos #'x)))))
                 (car #,(mark-strict-pos #'x))
                 (loop (cdr #,(mark-strict-pos #'x))))))]))

(define-syntax (profiled-reverse stx)
  (syntax-case stx ()
    [(_ lll)
     #`(let loop ([lst lll] [acc null])
         (if (null? #,(mark-strict-pos #'lst)) acc
             (loop (cdr #,(mark-strict-pos #'lst)) (cons (car #,(mark-strict-pos #'lst)) acc))))]))

(define-syntax (profiled-string-join stx)
  (syntax-parse stx
    [(_ ss sep (~seq #:before-last b))
     #`(string-join #,(mark-strict-pos #'ss) #,(mark-strict-pos #'sep) #:before-last #,(mark-strict-pos #'b))]))
(define-syntax (profiled-list->string stx)
  (syntax-case stx ()
    [(_ lst) #`(list->string (map (λ (x) #,(mark-strict-pos #'x)) lst))]))
  
;; ----------------------------------------------------------------------------
;; OO and classes
(define-syntax (profiled-new stx)
  (syntax-case stx ()
    [(_ cls . param) #`(new #,(mark-strict-pos #'cls) . param)]))
(define-syntax (profiled-send stx)
  (syntax-case stx ()
    [(_ obj method) #`(send #,(mark-strict-pos #'obj) method)]))
;; ----------------------------------------------------------------------------
;; 2htdp profiled functions

#;(define-syntax (profiled-big-bang stx)
  (syntax-case stx ()
    [(_ e ...)
     (with-syntax ([(stricte ...) (map mark-strict-pos (syntax->list #'(e ...)))])
       #'(big-bang stricte ...))]))
;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------
;; ---------- post processing functions
;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;; anum summary:
;; maps anums to final tally of unused, uses and creates prevented
(define summary-unused (make-hash))
(define summary-uses (make-hash))
(define summary-creates (make-hash))
;; maps anums to list of weights
(define summary-weights (make-hash))
(define (print-summary)
  (printf "\n===== ")
  (printf "Final Summary: Expressions to delay (sorted according to dependency)")
  (printf " =====\n")
  (define (print-anum-summary anum)
    (define num-creates-prevented (hash-ref summary-creates anum 0))
    (unless (zero? num-creates-prevented)
      (define num-vnums (set-count (hash-ref anum-to-vnums anum (set))))
      (define num-unused (hash-ref summary-unused anum 0))
      (printf "~a:\n" (anum->str anum))
      (printf "- delaying ~a/~a unused prevents ~a creates, max weight = ~a\n"
              num-unused num-vnums num-creates-prevented 
              (real->decimal-string (apply max (hash-ref summary-weights anum))
               #;(/ (+ num-unused num-creates-prevented) 
                                       num-unused)
                                    2))))
  (for-each print-anum-summary (reverse delayed-anums)))

;; ----------------------------------------------------------------------------
;; struct anum-info 
;(struct anum-info (anum vnums unused uses created wgt))
(struct anum-info (anum vnums unused uses num-created wgt))
(define (calc-weight unused uses created)
  (safe-/ (set-count (set-union unused created)) 
          (set-count (set-subtract unused created))))
(define (anum-info-cmp ai1 ai2)
  (match-define (anum-info _ _ _ uses1 _ wgt1) ai1)
  (match-define (anum-info _ _ _ uses2 _ wgt2) ai2)
  (if (= wgt1 wgt2)
      (> uses1 uses2)
      (> wgt1 wgt2)))
(define (print-anum-info ai)
  (match-define (anum-info anum vnums unused uses num-created wgt) ai)
  (define num-vnums (set-count vnums))
  (unless (zero? num-vnums) ; omit from results anums with 0 created vnums
    (define num-unused (set-count unused))
    ;(define num-created (set-count created))
    (printf "~a: used ~a/~a created values\n" 
            (anum->str anum) (- num-vnums num-unused) num-vnums)
    (unless (zero? num-unused)
      (printf "- delaying ~a unused would prevent ~a uses and ~a creates, weight = ~a\n"
              num-unused uses num-created (real->decimal-string wgt 2))
      (newline))))
;; get-anum-info
;; assumes anum exists
(define (get-anum-info anum vnums unused-vnums)
;  (define vnums         (set-filter vnum-exists? (hash-ref anum-to-vnums anum)))
;  (define unused-vnums  (set-filter vnum-unused? vnums))
;  (define uses          (apply + (set-map unused-vnums get-uses-by-vnum)))
  (define uses 0)
  (define created-vnums ; integer-set of created vnums (does not incl unused)
    (foldr 
     (λ (vnum acc) 
       (define vnum-end (sub1 (hash-ref sub-vnums vnum (add1 vnum))))
       (if (> (add1 vnum) vnum-end)
           acc
           (is:union acc (is:make-range (add1 vnum) vnum-end))))
     (is:make-range)
     (set->list unused-vnums))
    #;(apply set-union (set) (set-map unused-vnums get-vnums-created-by-vnum)))
  (define created+unused 
    (foldr (λ (vnum acc) (is:union acc (is:make-range vnum))) created-vnums (set->list unused-vnums)))
  (define num-created-vnums (for/sum ([vnum created+unused] #:when (vnum-exists? vnum)) 1))
  (define num-independent-unused-vnums
    (count (λ (vnum) (not (is:member? vnum created-vnums))) (set->list unused-vnums)))
  (define wgt (safe-/ num-created-vnums num-independent-unused-vnums))
  (anum-info anum vnums unused-vnums uses num-created-vnums wgt
             #;(calc-weight unused-vnums uses created-vnums)))
(define (get-vnums-of-anum anum)
  (set-filter vnum-exists? (hash-ref anum-to-vnums anum)))
(define (get-unused-vnums vnums)
  (set-filter vnum-unused? vnums))
  
;; tracks the order (last in list is first) in which anums are processed
(define delayed-anums null)

;; ----------------------------------------------------------------------------
;; --------- profile-results: prints final output of the profiler
;; ----------------------------------------------------------------------------
(define (profile-results)
  (printf "\n---------------------------- ")
  (printf "Initial Profiling Data")
  (printf " ----------------------------\n")
  (print-total-vnums)
  
  (printf "*** = simulating delay of this expression\n\n")
  
  (let LOOP ([round-num 0])
    (define vnums-lst (map get-vnums-of-anum all-existing-anums))
    (define unused-vnums-lst (map get-unused-vnums vnums-lst))
    (define anum-infos
      (for/list ([anum all-existing-anums]
                 [vnums vnums-lst]
                 [unused-vnums unused-vnums-lst]
                 #:unless (and SHOW-UNUSED-ONLY
                               (set-empty? unused-vnums)))
        (get-anum-info anum vnums unused-vnums))
      #;(for/list ([anum (get-total-anums)]
                   #:when (anum-exists? anum))
          (get-anum-info anum)))
    ;; first sort is by weight
    ;; second sort pushes already delayed anums to the top
    ;; (first delayed gets priority)
    (unless (null? anum-infos)
      (define sorted-anum-infos 
        (sort
         (sort anum-infos anum-info-cmp)
         > #:key (match-lambda 
                   [(anum-info anum _ unused _ _ _)
                    (if (zero? (set-count unused))
                        -1
                        (list-index anum delayed-anums -1))])))

      ;(match-define (anum-info anum _ unused _ created wgt) (car sorted-anum-infos))
      (match-define 
        (anum-info anum _ unused _ num-created wgt)
        (car sorted-anum-infos))
      
      (define num-unused (set-count unused))
      (unless (zero? num-unused)
        (printf "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ")
        (printf "Analysis Summary: Round ~a" round-num)
        (printf " ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
        
        (unless (member anum delayed-anums)
          (set! delayed-anums (cons anum delayed-anums)))
        (newline)
        (printf "***")
        (for-each print-anum-info sorted-anum-infos)
        (newline)
        ;      (print-anum-info (car sorted-anum-infos))
        (set-for-each unused erase!-vnum)
        (hash-update! summary-unused anum (λ (x) (+ x num-unused)) 0)
        ;(hash-update! summary-creates anum (λ (x) (+ x (set-count created))) 0)
        (hash-update! summary-creates anum (λ (x) (+ x num-created)) 0)
        (hash-update! summary-weights anum (λ (wgts) (cons wgt wgts)) null)
        (LOOP (add1 round-num)))))

  (print-summary))


(define (unprofile v)
  (match v
    [(profiled v _) (unprofile v)]
    [(cons x y) (cons (unprofile x) (unprofile y))]
    [v v]))
(define (unprofile-list-structure v)
  (match v
    [(profiled v _) (unprofile-list-structure v)]
    [(cons x y) (cons x (unprofile-list-structure y))]
    [v v]))
     

(define-syntax (mbegin stx)
  (syntax-case stx ()
    [(_ d ... e)
     #`(#%module-begin 
        d ... 
;        (unprofile #,(mark-top-level-pos #'e))
;        (pretty-print (syntax->datum (expand #'e)))
        (call-with-exception-handler 
         (λ (ex) ;; report partial results on break or error
           (set!-total-anums) 
           (time (profile-results))
           ex)
         (thunk (time (unprofile #,(mark-strict-pos (mark-arg-pos #'e))))))
;;;;;        (time (unprofile #,(mark-strict-pos (mark-arg-pos #'e))))
     
;   (hash-for-each 
;    anum-stx-info 
;    (λ (anum s) 
;      (display anum)
;      (match s [(syntx stx ln col) (display " ") (displayln stx)])))
        (set!-total-anums)
      (time (profile-results)))]))
;#lang racket
#lang s-exp "../../lazy-profile.rkt"

;(require "string-helpers.rkt")
(define (str-empty? str) (string=? str ""))
(define (str-fst str) (string-ref str 0))
(define (str-rst str) (substring str 1))

;(require (only-in racket [string mk-string]))
(define (mk-string x) (string x))
;(provide (all-defined-out))

;; Parsec with errors messages

;; A [Parser X] is a function State -> [Consumed X]

;; A State is a (State String Pos)
;; - parsers consume a State
(struct State (string pos) #:transparent)

;; A Message is a (Msg Pos String [List String])
(struct Msg (pos string strings) #:transparent)
  

;; A [Consumed X] is one of:
;; - (Consumed [Reply X])
;; - (Empty [Reply X])
(struct Consumed (reply) #:transparent) ;; should be lazy? (see >>= def)
(struct Empty (reply) #:transparent)

;; A [Reply X] is one of:
;; - (Ok X State Message)
;; - (Error Message)
(struct Ok (parsed rest msg) #:transparent)
(struct Error (msg) #:transparent)

;; creates a parser that consumes no input and returns x
(define (return x)
  (λ (state) (Empty (Ok x state (Msg 0 "" null))))
  #;(match-lambda 
    [(and state (State _ pos)) 
     (Empty (Ok x state (Msg pos "" null)))]))


;; creates a parser that consumes 1 char if it satisfies predicate p?
(define (satisfy p?)
  (λ (state)
    (match state
;  (match-lambda 
    [(State input pos)
     (if (str-empty? input)
         (Empty (Error (Msg pos "end of input" null)))
         (let ([c (str-fst input)]
               [cs (str-rst input)])
           (if (p? c)
               (let* ([new-pos (add1 pos)]
                      [new-state (State cs new-pos)])
                 (Consumed (Ok c new-state (Msg new-pos "" null))))
               (Empty (Error (Msg pos (mk-string c) null #;(list "input satisfying given predicate")))))))])))

(define (noneOf str)
  (define (char=any c s)
    (if (str-empty? s)
        #f
        (or (char=? c (str-fst s))
            (char=any c (str-rst s)))))
  (define (str->strs)
    (format-exp (map (λ (x) (string-append "\"" (mk-string x) "\"")) (string->list str))))
  (λ (state)
    (match ((satisfy (λ (c) (not (char=any c str)))) state)
      [(Consumed (Error (Msg pos inp exp)))
       (Consumed (Error (Msg pos inp (cons (string-append "none of " (str->strs)) exp))))]
      [(Empty (Error (Msg pos inp exp)))
       (Empty (Error (Msg pos inp (cons (string-append "none of " (str->strs))
                                        exp))))]
      [ok ok])))
       



;; creates a parser that combines two parsers p and f
;; - if p succeeds but does not consume input, then f determines result
;; - if p succeeds and consumes input, return Consumed with thunk that delays
;;   parsing from f
(define (>>= p f)
  (λ (input)
    (match (p input)
      [(Empty reply)
       (match reply
         [(Ok x rest msg1)
          (match ((f x) rest)
            [(Empty (Error msg2)) (mergeError msg1 msg2)]
            [(Empty (Ok x inp msg2)) (mergeOk x inp msg1 msg2)]
            [consumed consumed])]
         [err (Empty err)])]
      [(Consumed reply1)
       (Consumed ; arg to Consumed constructor should be delayed
;        (lazy
         (match reply1 ;(force reply1)
           [(Ok x rest msg1)
            (match ((f x) rest)
              [(Consumed reply2) reply2]
              [(Empty (Error msg2)) (Error (merge msg1 msg2))]
              [(Empty ok) ok]
              #;[(Empty reply2) reply2])]
           [error error]))])))

;; <|> choice combinator
(define (<or>2 p q)
  (λ (state)
    (match (p state)
      [(Empty (Error msg1))
       (match (q state)
         [(Empty (Error msg2)) (mergeError msg1 msg2)]
         [(Empty (Ok x inp msg2)) (mergeOk x inp msg1 msg2)]
         #;[(Consumed (Ok x inp msg2)) (mergeConsumed x inp msg1 msg2)]
         [consumed consumed])]
      [(Empty (Ok x inp msg1))
       (match (q state)
         [(Empty (Error msg2)) (mergeOk x inp msg1 msg2)]
         [(Empty (Ok _ _ msg2)) (mergeOk x inp msg1 msg2)]
         #;[(Consumed (Ok x inp msg2)) (mergeConsumed x inp msg1 msg2)]
         [consumed consumed])]
      #;[(Consumed (Ok x inp msg1))
       (match (q inp)
         [(Empty (Error msg2)) (mergeError msg1 msg2)]
         [consumed consumed])]
      [consumed consumed])))
(define (mergeConsumed x inp msg1 msg2) (Consumed (Ok x inp (merge msg1 msg2))))
(define (mergeOk x inp msg1 msg2) (Empty (Ok x inp (merge msg1 msg2))))
(define (mergeError msg1 msg2) (Empty (Error (merge msg1 msg2))))
;(define/match (merge msg1 msg2)
(define (merge msg1 msg2)
  (match* (msg1 msg2)
  [((Msg pos inp exp1) (Msg _ _ exp2))
   (Msg pos inp (append exp1 exp2))]))
                      
(define (foldl f acc lst)
  (if (null? lst)
      acc
      (foldl f (f (car lst) acc) (cdr lst))))
;; assumes (length args) >= 2
(define (<or> . args)
  (foldl (λ (p acc) (<or>2 acc p)) (car args) (cdr args)))

;; lookahead
(define (try p)
  (λ (state)
    (match (p state)
      [(Consumed (Error msg)) (Empty (Error msg))]
      [other other])))



;; parse with p 0 or more times
(define (many p)
  (<or> (>>= p
             (λ (x) (>>= (many p) (λ (xs) (return (cons x xs))))))
        (return null)))

;; parse with p 1 or more times
(define (many1 p)
  (>>= p 
       (λ (x) (>>= (<or> (many1 p) (return null))
                   (λ (xs) (return (cons x xs)))))))




(define (sepBy1 p sep)
  (>>= p (λ (x) (>>= (many (>>= sep (λ _ p))) (λ (xs) (return (cons x xs)))))))
(define (sepBy p sep) (<or> (sepBy1 p sep) (return null)))

(define (endBy p end) (<or> (many (>>= p (λ (x) (>>= end (λ _ (return x))))))
                            (return null)))

(define (<?> p exp)
  (λ (state)
    (match (p state)
      [(Empty (Error msg)) (Empty (Error (expect msg exp)))]
      [(Empty (Ok x st msg)) (Empty (Ok x st (expect msg exp)))]
      [other other])))
(define (expect msg exp)
  (match msg
    [(Msg pos inp _) (Msg pos inp (list exp))]))

;; creates a parser that parses char c
(define (char c) (<?> (satisfy (curry char=? c)) (string-append "\"" (mk-string c) "\"")))
(define letter (<?> (satisfy char-alphabetic?) "letter"))
(define digit (<?> (satisfy char-numeric?) "digit"))

(define (parse-string str)
  (if (str-empty? str)
      (return null)
      (>>= (char (str-fst str)) (λ _ (parse-string (str-rst str))))))

;; parser that only succeeds on empty input
(define eof
  (<?>
   (λ (state)
     (match state
       [(State inp pos)
        (if (str-empty? inp) 
            (Empty (Ok null state (Msg pos "" null)))
            (Empty (Error (Msg pos "non-empty input" null))))]))
   "end-of-file"))
(define eol (<?> (<or> (try (parse-string "\n\r"))
                       (try (parse-string "\r\n"))
                       (try (parse-string "\n"))
                       (try (parse-string "\r")))
                 "end-of-line"))

(define (identifier x) (parse (<?> (many1 (<or> letter digit (char #\_))) "identifier") x))

(define (format-exp exp) (string-join exp ", " #:before-last " or "))
(define (parse p inp) 
  (define res (p (State inp 0)))
  res
  #;(if (Empty? res) res
      (if (Error? (Consumed-reply res)) res
          (Ok-parsed (Consumed-reply res))))
  #;(match (p (State inp 0))
    [(Empty (Error (Msg pos msg exp)))
     (error 'parse-error 
            "at pos ~a\nunexpected ~a: expected ~a" 
            pos msg (format-exp exp))]
    [(Consumed (Error (Msg pos msg exp)))
     (error 'parse-error 
            "at pos ~a\nunexpected ~a: expected ~a" 
            pos msg (format-exp exp))]
    [x x]))
  
;; csv-parser-with-errors.rkt ----------------------------------------------
;; many1 means cells cannot be empty
(define cellContent (many (noneOf ",\n\r")))

(define remainingCells
  (<or> (>>= (char #\,) (λ _ cells))
        (return null)))

(define cells
  (>>= cellContent (λ (x) (>>= remainingCells (λ (xs) (return (cons x xs)))))))

;; a line must end in \n
(define line
  (>>= cells (λ (result) (>>= eol (λ _ (return result))))))

;; result is list of list of chars

(define csv
;  (>>= (many1 line) (λ (result) (>>= (char #\null) (λ _ (return result))))))
  (>>= (many line) (λ (result) (>>= eof (λ _ (return result))))))


(define (csvFile filename)
  (parse csv (with-input-from-file filename port->string)))

(csvFile "csv-example")
#lang s-exp "../../lazy-profile.rkt"

;; A [Parser X] is a function String -> [Consumed X]

;; A [Consumed X] is one of:
;; - (Consumed [Reply X])
;; - (Empty [Reply X])
(struct Consumed (Reply) #:transparent) ;; should be lazy? (see >>= def)
(struct Empty (Reply) #:transparent)

;; A [Reply X] is one of:
;; - (Ok X String)
;; - Error
(struct Ok (fst rst) #:transparent)
(struct Err ())
(define Error (Err))
(define (Error? x) (eq? x Error))

;; creates a parser that consumes no input and returns x
(define (return x)
  (λ (input) (Empty (Ok x input))))

(define (str-empty? str) (string=? str ""))
(define (str-fst str) (string-ref str 0))
(define (str-rst str) (substring str 1))

;; creates a parser that consumes 1 char if it satisfies predicate p?
(define (satisfy p?)
  (λ (input)
    (if (str-empty? input)
        (Empty Error)
        (let ([c (str-fst input)])
          (if (p? c)
              (Consumed (Ok c (str-rst input)))
              (Empty Error))))))

(define (noneOf str)
  (define (char=any c s)
    (if (str-empty? s)
        #f
        (or (char=? c (str-fst s))
            (char=any c (str-rst s)))))
  (satisfy (λ (c) (not (char=any c str)))))

;; creates a parser that parses char c
(define (char c) (satisfy (λ (x) (char=? x c))))
(define letter (satisfy char-alphabetic?))
(define digit (satisfy char-numeric?))

;; creates a parser that combines two parsers p and f
;; - if p succeeds but does not consume input, then f determines result
;; - if p succeeds and consumes input, return Consumed with thunk that delays
;;   parsing from f
(define (>>= p f)
  (λ (input)
    (match (p input)
      [(Empty reply)
       (match reply
         [(Ok x rest) ((f x) rest)]
         [(Err) (Empty Error)])]
      [(Consumed reply1)
       (Consumed ; arg to Consumed constructor should be delayed
        (match reply1
          [(Ok x rest)
           (match ((f x) rest)
             [(Consumed reply2) reply2]
             [(Empty reply2) reply2])]
          [error error]))])))

;; <|> choice combinator
(define (<or> p q)
  (λ (input)
    (match (p input)
      [(Empty (Err)) (q input)]
      [(Empty ok)
       (match (q input)
         [(Empty _) (Empty ok)]
         [consumed consumed])]
      [consumed consumed])))

(define (string str)
  (if (str-empty? str)
      (return #t)
      (>>= (char (str-fst str)) (λ _ (string (str-rst str))))))

(define (many p)
  (<or> (>>= p
             (λ (x) (>>= (many p) (λ (xs) (return (cons x xs))))))
        (return null)))

(define (many1 p)
  (>>= p 
       (λ (x) (>>= (<or> (many1 p) (return null))
                   (λ (xs) (return (cons x xs)))))))

(define (identifier x) ((many1 (<or> letter (<or> digit (char #\_)))) x))

(define eol (char #\newline))

;; many1 means cells cannot be empty
(define cellContent (many (noneOf ",\n")))

(define remainingCells
  (<or> (>>= (char #\,) (λ _ cells))
        (return null)))

(define cells
  (>>= cellContent (λ (first) (>>= remainingCells (λ (next) (return (cons first next)))))))

;; a line must end in \n
(define line
  (>>= cells (λ (result) (>>= eol (λ _ (return result))))))

;; result is list of list of chars

(define csvFile
;  (>>= (many1 line) (λ (result) (>>= (char #\null) (λ _ (return result))))))
  (>>= (many line) (λ (result) (return result))))


(csvFile (with-input-from-file "csv-example" port->string))

#lang typed/racket
(require typed/test-engine/racket-tests)


;; ==== Problem 1 ====

(: catw : String (Listof String) -> String)
;; consume a separator and a list of strings and concatenate all strings with
;; separator between each pair
(define (catw string string-list)
  (cond
    [(empty? string-list) ""]
    [(= (length string-list) 1) (first string-list)]
    [else (string-append (first string-list)
                         string
                         (catw string (rest string-list)))]))
;; TESTS
(check-expect (catw "<*>" '("A" "B" "C")) "A<*>B<*>C")
(check-expect (catw ":" '("A" "B" "C")) "A:B:C")
(check-expect (catw ":" '("A")) "A")
(check-expect (catw ":" '()) "")

(: every : (All (a) (a -> Boolean) (Listof a) -> Boolean))
;; test if every item in a list passes the given test, stopping if any item fails
(define (every func list)
  (cond
    [(empty? list) #t]
    [(func (first list)) (every func (rest list))]
    [else #f]))
;; TESTS
(check-expect (every even? '(1 2 3)) #f)
(check-expect (every positive? '(1 2 3)) #t)
(check-expect (every positive? '()) #t)

(: discard : (All (a) (a -> Boolean) (Listof a) -> (Listof a)))
;; discard all items from a list that pass a given test
(define (discard func list)
  (cond
    [(empty? list) empty]
    [(func (first list)) (discard func (rest list))]
    [else (cons (first list) (discard func (rest list)))]))
;; TESTS
(check-expect (discard even? '(1 2 3)) '(1 3))
(check-expect (discard positive? '(1 2 3)) '())


;; ==== Problem 2 ====

(define-type Exp (U Literal Operation))

(define-struct Literal
  ([val : Integer])
  #:transparent)

(define-type Operator (U '+ '*))

(define-struct Operation
  ([op : Operator]
   [es : (Listof Exp)]
   [en : Exp])
  #:transparent)

; An example tree for later testing
; Should evaluate to 871 and (+ 1 (* 4 5 6 7) (* 10 (+ 1 2)))
(define stock-exp
        (Operation '+
                   (list (Literal 1)
                         (Operation '*
                                    (list (Literal 4)
                                          (Literal 5)
                                          (Literal 6))
                                    (Literal 7)))
                   (Operation '*
                              (list (Literal 10))
                              (Operation '+
                                         (list (Literal 1))
                                         (Literal 2)))))

(: eval-tree : Exp -> Integer)
;; evalutate an expression down to an integer value
(define (eval-tree exp)
  (match exp
    [(Literal val) val]
    [(Operation _ '() en) (eval-tree en)]
    [(Operation '+ (cons hd tl) en) (+ (eval-tree hd)
                                       (eval-tree (make-Operation '+
                                                                  tl
                                                                  (Literal 0)))
                                       (eval-tree en))]
    [(Operation '* (cons hd tl) en) (* (eval-tree hd)
                                       (eval-tree (make-Operation '*
                                                                  tl
                                                                  (Literal 1)))
                                       (eval-tree en))]))
;; TESTS
(check-expect (eval-tree stock-exp) 871)

(: unparse : Exp -> String)
;; convert an expression into a string that can be pasted into Racket REPL
(define (unparse exp)
  (local
    {(: unparse-list : (Listof Exp) -> String)
     ;; use unparse function on a list of expressions
     (define (unparse-list exp-list)
       (match exp-list
         ['() ""]
         [(cons hd '()) (unparse hd)]
         [(cons hd tl) (string-append (unparse hd)
                                      " "
                                      (unparse-list tl))]))}
    (match exp
      [(Literal val) (number->string val)]
      [(Operation op '() en) (string-append "("
                                           (symbol->string op)
                                           " "
                                           (unparse en)
                                           ")")]
      [(Operation op es en) (string-append "("
                                           (symbol->string op)
                                           " "
                                           (unparse-list es)
                                           " "
                                           (unparse en)
                                           ")")])))
;; TESTS
(check-expect (unparse stock-exp) "(+ 1 (* 4 5 6 7) (* 10 (+ 1 2)))")

(test)
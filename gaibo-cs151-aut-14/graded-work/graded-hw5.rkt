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
;; Evaluate =============================

;; Problem 1 
(check-expect (catw "<*>" '("A" "B" "C")) "A<*>B<*>C")
(check-expect (catw "i" '()) "")

(check-expect (every even? '(1 2 3)) #f)
(check-expect (every even? '(4 2 6)) #t)

(check-expect (every positive? '(1 2 3)) #t)
(check-expect (every positive? '(-1 2 3)) #f)

(check-expect (discard even? '(1 2 3)) '(1 3))
(check-expect (discard odd? '(1 2 3)) '(2))
(check-expect (discard positive? '(1 2 3)) '())
(check-expect (discard positive? '(-1 -2 -3)) '(-1 -2 -3))

;; Problem 2
(check-expect (eval-tree (Literal 2)) 2)
(check-expect (eval-tree (Operation '+ (list (Literal 3) 
              (Literal 4)) (Literal 4))) 11)
(check-expect (eval-tree (Operation '+ (list (Literal 6) (Literal -1)) 
              (Operation '* (list (Literal 8) (Literal 7)) (Literal 3)))) 173)
(check-expect (eval-tree (Operation '+ (list (Literal 1) (Literal 2)) (Literal 3))) 6)
(check-expect (eval-tree (Operation '* (list (Literal 1) (Literal 2)) (Literal 3))) 6)
(check-expect (eval-tree (Operation '+ '() (Literal 3))) 3)
(check-expect (eval-tree (Operation '* '() (Literal 3))) 3)
(check-expect (eval-tree (Literal 3)) 3)

(: Test-exp-2 : Exp)
;; (+ 1 (* 4 5 6 7) (* 10 (+ 1 2)))
(define Test-exp-2
  (Operation '+ (list (Literal 1) (Operation '* (list (Literal 4) (Literal 5)
                                                     (Literal 6)) (Literal 7)))
             (Operation '* (list (Literal 10)) (Operation '+ (list (Literal 1))
                                                        (Literal 2)))))
(: Test-exp-1 : Exp)
;; (+ 7 8 -4 0)
(define Test-exp-1
  (Operation '+ (list (Literal 7) (Literal 8) (Literal -4)) (Literal 0)))
(: Test-exp-3 : Exp)
;; (+ 5 3 (* 4 15 (* 3 2 (+ 1 4 5) 2) -2) 18)
(define Test-exp-3
  (Operation '+ 
  (list (Literal 5) (Literal 3) (Operation '* (list (Literal 4) (Literal 15) 
  (Operation '* (list (Literal 3) (Literal 2)  (Operation '+ (list (Literal 1)
  (Literal 4)) (Literal 5)))  (Literal 2)))  (Literal -2))) (Literal 18)))

(check-expect (unparse (Literal 13)) "13")
(check-expect (unparse Test-exp-1) "(+ 7 8 -4 0)")
(check-expect (unparse Test-exp-2) "(+ 1 (* 4 5 6 7) (* 10 (+ 1 2)))")
(check-expect (unparse Test-exp-3) 
              "(+ 5 3 (* 4 15 (* 3 2 (+ 1 4 5) 2) -2) 18)")
;; ====== correctness

;; === correctness ===

;; problem 1 (catw)                  -/ 4
;; problem 1 (every)                 -/ 4
;; problem 1 (discard)               -/ 3

;; problem 2 (eval-tree)             -/ 6
;; problem 2 (unparse)               -/ 5

;; === style ===

;; code layout                       -/ 4
;; identifiers are well named        -/ 4
;; program decomposition (helpers)   -/ 2

;; contracts (types)                 -/ 4
;; well-written purposes             -/ 4
;; adequate tests                    -/ 4

;; clarity (clear logic)             -/ 3

;; svn used correctly                -/ 3

;; _total-score_                    50/ 50

;; grader: Nikita Mishra



#lang typed/racket
(require typed/test-engine/racket-tests)


;; ==== Problem 1 ====

(define-type Exp (U Literal Operation Local BoundId))

(define-struct Literal
  ([val : Integer])
  #:transparent)

(define-type Operator (U '+ '*))

(define-struct Operation
  ([op : Operator]
   [es : (Listof Exp)]
   [en : Exp])
  #:transparent)

(define-type Identifier String)

(define-struct Local
  ([id  : Identifier]
   [val : Exp]
   [in  : Exp])
  #:transparent)

(define-struct BoundId
  ([id : Identifier])
  #:transparent)

(define-struct Pair
  ([first : String]
   [second : Integer])
  #:transparent)

(define-type Bindings (Listof Pair))

; ------------------------------------------------------------------------------
; Some instructor-given test expressions for later use

; Should evaluate to 7
(define stock-exp-1
  (Operation '+
             (list (Literal 1))
             (Local "x"
                    (Operation '*
                               (list (Literal 2))
                               (Literal 3))
                    (BoundId "x"))))

; Should evaluate to 42
(define stock-exp-2
  (Local "x"
         (Operation '+
                    (list (Literal 5))
                    (Literal 2))
         (Local "y"
                (Operation '*
                           (list (Literal 2))
                           (Literal 3))
                (Operation '*
                           (list (BoundId "x"))
                           (BoundId "y")))))
; ------------------------------------------------------------------------------

(: bindings-matcher : String Bindings -> Integer)
;; check an id-value map (the so-called Bindings, which is a list of Pairs) and
;; return the value (the Integer) which matches the desired id (the String)
(define (bindings-matcher id2m bindings)  ; id2m = id to match
  (match bindings
    ['() (error "this should not happen. undefined id")]
    [(cons hd tl) (match hd
                    [(Pair id val) (if (equal? id id2m)
                                       val
                                       (bindings-matcher id2m tl))])]))
;; TESTS
(check-expect (bindings-matcher "x" (list (Pair "y" 2)
                                          (Pair "x" 5)
                                          (Pair "z" 1)))
              5)
(check-error (bindings-matcher "a" (list (Pair "y" 2)
                                         (Pair "x" 5)
                                         (Pair "z" 1)))
             "this should not happen. undefined id")

(: eval-tree : Bindings Exp -> Integer)
;; consume a value of type Bindings that maps Identifiers to values and an Exp
;; and output the numerical result
;; NOTE: I essentially took my code from HW5 and added to additional cases that
;; addressed the two new structs
(define (eval-tree bindings exp)
  (match exp
    [(Literal val) val]
    [(Operation _ '() en) (eval-tree bindings en)]
    [(Operation '+ (cons hd tl) en) (+ (eval-tree bindings hd)
                                       (eval-tree bindings
                                                  (make-Operation '+
                                                                  tl
                                                                  (Literal 0)))
                                       (eval-tree bindings en))]
    [(Operation '* (cons hd tl) en) (* (eval-tree bindings hd)
                                       (eval-tree bindings
                                                  (make-Operation '*
                                                                  tl
                                                                  (Literal 1)))
                                       (eval-tree bindings en))]
    [(Local id val in) (eval-tree (cons (make-Pair id
                                                   (eval-tree bindings
                                                              val))
                                        bindings)
                                  in)]
    [(BoundId id2m) (bindings-matcher id2m bindings)]))
;; TESTS
(check-expect (eval-tree '() stock-exp-1) 7)
(check-expect (eval-tree '() stock-exp-2) 42)


;; ==== Problem 2 ====

(define-type Integrator ((Real -> Real) Real Real Integer -> Real))

(: integrate-list : Integrator)
;; consume a function, a left bound, a right bound, a number of rectangles to
;; sum, and output the approximate left Riemann sum area
(define (integrate-list function left right n)
  (local
    {(define increment (/ (- right left) n))
     (: create-xlist : (Listof Real) Integer -> (Listof Real))
     ;; generate a list of left x-values at which the function can be evaluated
     (define (create-xlist acc counter)
       (cond
         [(not (= counter 0))
          (match acc
            ['() (create-xlist (cons left acc)
                               (- counter 1))]
            [(cons hd _) (create-xlist (cons (+ hd increment) acc)
                                       (- counter 1))])]
         [else (reverse acc)]))
     ; NOTE: on second thought the above could have been made with build-list
     (: mult-increment : Real -> Real)
     ;; multiply a number by the increment
     (define (mult-increment number)
       (* increment number))}
    (foldl + 0 (map mult-increment (map function (create-xlist '() n))))))
;; TESTS
(check-within (integrate-list sqr 1 2 5) 2.04 0.001)
(check-within (integrate-list add1 1 2 5) 2.4 0.001)

(: integrate-acc : Integrator)
;; use an accumulator to achieve a tail-recursive version of left Riemann sums
(define (integrate-acc function left right n)
  (local
    {(define increment (/ (- right left) n))
     (: accumulation : Real Real -> Real)
     ;; accumulate squares with counter serving as both counter and the x-value
     (define (accumulation acc counter)
       (cond
         [(not (= counter 0))
          (accumulation (+ (* (function counter) increment) acc)
                           (- counter increment))]
         [else acc]))}
    (accumulation 0 (- right increment))))
    ; initialize with acc = 0 since we're summing and counter = right - increment
    ; because we want the left offset
;; grader: should compare your "counter" with left to decide the time to terminating
;; the recursion.  when counter is equal to left, still need to do the last 
;; iteration.

;; TESTS
(check-within (integrate-list sqr 1 2 5) 2.04 0.001)
(check-within (integrate-list add1 1 2 5) 2.4 0.001)


;; ==== Problem 3 ====

(: is-palindrome? : (Vectorof Integer) -> Boolean)
;; determine whether a given vector is palindromic
(define (is-palindrome? vector)
  (local
    {(define last-index (- (vector-length vector) 1))
     (define num-of-pairs (quotient (vector-length vector) 2))
     ; ^ number of pairs that have to match
     (: ends-matcher : Integer -> Boolean)
     ;; try matching the given number elements on the ends of the vector
     (define (ends-matcher counter)
       (cond
         [(= counter -1) #t]  ; -1 is used since for vectors index 0 must pass
         [(= (vector-ref vector counter)
             (vector-ref vector (- last-index counter)))
          (ends-matcher (- counter 1))]
         [else #f]))}
    (ends-matcher num-of-pairs)))
;; TESTS
(check-expect (is-palindrome? '#(1 2 3 2 1)) #t)
(check-expect (is-palindrome? '#(1 2 2 1)) #t)
(check-expect (is-palindrome? '#(17 24 36 24 17)) #t)
(check-expect (is-palindrome? '#(1 2 3 4)) #f)
(check-expect (is-palindrome? '#(1 2 3 2)) #f)
(check-expect (is-palindrome? '#(1 2 3 2 1 0)) #f)
(check-expect (is-palindrome? '#(0)) #t)
(check-expect (is-palindrome? '#(0 0)) #t)
(check-expect (is-palindrome? '#(0 1)) #f)
(check-expect (is-palindrome? '#(1 0)) #f)

(test)




;; ========= grader's tests =========

;; === problem 1
(define grader-empty-map '())

(define grader-test-exp
  (Operation '+
             (list (Literal 1)
                   (Operation '* (list (Literal 4) (Literal 5) (Literal 6)) (Literal 7)))
             (Operation '* (list (Literal 10)) (Operation '+ (list (Literal 1)) (Literal 2)))
             ))

(check-expect (eval-tree grader-empty-map grader-test-exp) 871)

(define grader-test-bind1
  (Operation '+ (list (Literal 1))
             (Local "x" (Literal 2) (BoundId "x"))))

(check-expect (eval-tree grader-empty-map grader-test-bind1) 3)

(define grader-test-lexscope
  (Operation '+ (list (Literal 1))
             (Local "x" (Literal 2)
                    (Local "x" (Literal 3)
                           (BoundId "x")))))

(check-expect (eval-tree grader-empty-map grader-test-lexscope) 4)

(define grader-test-ortho
  (Operation '+ (list (Literal 1))
             (Local "x" (Literal 2)
                    (Local "y" (Literal 3)
                           (BoundId "x")))))

(check-expect (eval-tree grader-empty-map grader-test-ortho) 3)

(define grader-test-outofscope
  (Operation '+ (list (Literal 1)
                      (Local "x" (Literal 2)
                             (BoundId "x")))
             (BoundId "x")))

(check-error (eval-tree grader-empty-map grader-test-outofscope) "unbound identifier")

(define test-recursive
  (Operation '+ (list (Literal 1))
             (Local "x" (BoundId "x")
                    (BoundId "x"))))

(check-error (eval-tree grader-empty-map test-recursive) "unbound identifier")

(define grader-test-propagate
  (Operation '+ (list (Literal 1))
             (Local "x" (Literal 2)
                    (Operation '+ (list (Literal 3)) (BoundId "x")))))

(check-expect (eval-tree grader-empty-map grader-test-propagate) 6)

;; === problem 2

(: grader-test-fun1 (-> Real Real))
(define (grader-test-fun1 x)
  (+ x 1))

(: grader-test-fun2 (-> Real Real))
(define (grader-test-fun2 x)
  (+ (* 3 x x) (* -2 x) -1))

(check-expect (integrate-list grader-test-fun1 0 2 2) 3)
(check-within (integrate-list grader-test-fun1 0 2 4) 7/2 0.00001)
(check-within (integrate-list grader-test-fun2 0 2 100) 2 0.1)
(check-within (integrate-list grader-test-fun2 0 2 10000) 2 0.001)
(check-within (integrate-list grader-test-fun2 0 4 100) 44 0.8)
(check-within (integrate-list grader-test-fun2 0 4 10000) 44 0.008)

(check-expect (integrate-acc grader-test-fun1 0 2 2) 3)
(check-within (integrate-acc grader-test-fun1 0 2 4) 7/2 0.00001)
(check-within (integrate-acc grader-test-fun2 0 2 100) 2 0.1)
(check-within (integrate-acc grader-test-fun2 0 2 10000) 2 0.001)
(check-within (integrate-acc grader-test-fun2 0 4 100) 44 0.8)
(check-within (integrate-acc grader-test-fun2 0 4 10000) 44 0.008)

;; === problem 3

(check-expect (is-palindrome? (vector 1 2 3 3 2 1)) #t)
(check-expect (is-palindrome? (vector 1 2 3 2 1)) #t)
(check-expect (is-palindrome? (vector 1 2 3 3 2 1 1)) #f)
(check-expect (is-palindrome? (vector 1 1 2 3 3 2 1)) #f)
(check-expect (is-palindrome? (vector 0 -1 2 -1 0)) #t)
(check-expect (is-palindrome? (vector 1 2 3 1 2 3 2 1)) #f)
(check-expect (is-palindrome? (vector 1 2 3 2 -3 2 1)) #f)

(test)

;; ====== evaluation ======

;; === correctness ===

;; problem 1 (eval-tree)            20/ 20

;; problem 2 (integrade-list)        7/ 8
;; may use build-list to create x's

;; problem 2 (integrate-acc)         7/ 8
;; incorrect base case. see comments in the code.

;; problem 3 (is-palindrome?)        8/ 8

;; === style ===

;; code layout                       8/ 8
;; identifiers are well named        8/ 8
;; program decomposition (helpers)   4/ 4

;; contracts (types)                 8/ 8
;; well-written purposes             8/ 8
;; adequate tests                    8/ 8

;; clarity (clear logic)             6/ 6

;; svn used correctly                6/ 6

;; _total-score_                   98/ 100

;; grader: Liwen Zhang


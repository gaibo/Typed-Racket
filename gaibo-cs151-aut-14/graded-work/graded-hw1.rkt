#lang typed/racket
(require typed/test-engine/racket-tests)


;; Problem 1

(: cm->in : Exact-Rational -> Exact-Rational)
;; convert centimeters to inches with input of a value in centimeters (cm)
(define (cm->in cm)
  (* cm 100/254))
;; TESTS
;; using check-expect since result is exact
(check-expect (cm->in 254/100) 1)
(check-expect (cm->in 254/10) 10)

(: in->cm : Exact-Rational -> Exact-Rational)
;; convert inches to centimeters with input of a value in inches (in)
(define (in->cm in)
  (* in 254/100))
;; TESTS
(check-expect (in->cm 1) 254/100)
(check-expect (in->cm 100/254) 1)

(: molecular-weight-hydrocarbon : Integer Integer -> Integer)
;; calculate the molecular weight of a hydrocarbon with input 
;; of number of carbon atoms (C) and number of hydrogen atoms (H)
(define (molecular-weight-hydrocarbon C H)
  (+ (* C 12) (* H 1)))
;; TESTS
(check-expect (molecular-weight-hydrocarbon 2 4) 28)
(check-expect (molecular-weight-hydrocarbon 3 8) 44)

(: eval-quadratic : Real Real Real Real -> Real)
;; calculate y-value of quadratic function with inputs a, b, c,
;; and x according to the equation y = ax^2 + bx + c
(define (eval-quadratic a b c x)
  (+ (* a x x) (* b x) c))
;; TESTS
;; using check-within due to inexactness of Real
(check-within (eval-quadratic 1 2 4 1) 7 0.01)
(check-within (eval-quadratic 2 1 3 2) 13 0.01)


;; Problem 2

(: valid-celsius? : Real -> Boolean)
;; verify whether a temperature value in degrees Celsius (degCel) is possible
(define (valid-celsius? degCel)
  (> degCel -273.15))
;; TESTS
(check-expect (valid-celsius? 100) #t)
(check-expect (valid-celsius? -300) #f)

(: within? : Real Real Real -> Boolean)
;; determine whether two numbers a and b are within a specified distance
;; (epsilon) of each other
(define (within? epsilon a b)
  (and (> epsilon (- a b)) (> epsilon (- b a))))  ; works since epsilon must reasonably be positive, and one of those subtraction expressions will always be negative and yield true
;; TESTS
(check-expect (within? 3 2 4) #t)
(check-expect (within? 4 3.5 10.5) #f)

(: on-quadratic? : Real Real Real Real Real Real -> Boolean)
;; determine whether the specified point (x,y) is vertically within a tolerance
;; (epsilon) of the quadratic curve ax^2 + bx + c
(define (on-quadratic? a b c x y epsilon)
  (and (> epsilon (- (f-of-x a b c x) y)) 
       (> epsilon (- y (f-of-x a b c x)))))
;; TESTS
(check-expect (on-quadratic? 1 2 4 2 13 3) #t)
(check-expect (on-quadratic? 1 4 4 1 13 1) #f)

(: f-of-x : Real Real Real Real -> Real)
;; [AUXILIARY] evaluate the y-value for an x-value on the quadratic curve in
;; on-quadratic?
(define (f-of-x a b c x)
  (+ (* a x x) (* b x) c))
;; [AUXILIARY] TESTS
(check-within (f-of-x 1 2 4 2) 12 0.01)
(check-within (f-of-x 1.5 2.2 4.5 2.7) 21.375 0.01)


;; Problem 3

(: eggs-to-buy : Integer -> Integer)
;; determine the number of eggs you need to buy, accounting for breakage and
;; container size, if you still need n eggs by the end for an omelet
;; NOTE: most economical method is found each time
(define (eggs-to-buy n)
  (cond
    [(< (/ (eggs-needed n) 12) 1) (* 12 1)]  ; if one carton of 12 will do the job
    [(< (- (eggs-needed n) (* 18 (large-cartons-floor n))) 12) (+ (* 18 (large-cartons-floor n)) (* 12 1))]  ; for as many 18-cartons as possible while still ending with a 12-carton
    [else (* 18 (large-cartons-ceiling n))]))  ; if neither of the previous two are possible for saving money, default to buying all 18-cartons
;; TESTS
(check-expect (eggs-to-buy 35) 48)
(check-expect (eggs-to-buy 24) 30)

(: large-cartons-floor : Integer -> Integer)
;; [AUX 1] round down how many 18-cartons are needed
(define (large-cartons-floor n)
  (exact-floor (/ (eggs-needed n) 18)))
;; [AUX 1] TESTS
(check-expect (large-cartons-floor 35) 2)
(check-expect (large-cartons-floor 60) 4)

(: large-cartons-ceiling : Integer -> Integer)
;; [AUX 2] round up how many 18-cartons are needed
(define (large-cartons-ceiling n)
  (exact-ceiling (/ (eggs-needed n) 18)))
;; [AUX 2] TESTS
(check-expect (large-cartons-ceiling 35) 3)
(check-expect (large-cartons-ceiling 60) 5)

(: eggs-needed : Integer -> Integer)
;; [AUX 3] determine number of eggs needed without accounting for container
;; size yet
(define (eggs-needed n)
  (exact-ceiling (* n 100/83)))
;; [AUX 3] TESTS
(check-expect (eggs-needed 8) 10)
(check-expect (eggs-needed 35) 43)


;; Problem 4

(: g : Integer -> Exact-Rational)
;; return the sum of the first 2n positive integers divided by the sum of the
;; first n positive integers given n
(define (g n)
  (/ (* 2 (+ (* 2 n) 1)) (+ n 1)))  ; expression pre-simplified by hand to avoid auxiliary functions
;; TESTS
(check-expect (g 2) 10/3)
(check-expect (g 5) 11/3)

(test)

;; evaluation

;; You should use 'within?' and 'eval-quadratic' in your definition of
;; 'on-quadratic?'.  Writing the same code twice when you can write it
;; once and re-use it is bad style.

;; Some of your lines are too long.  Keep them under 80 characters.

;; For 'eggs-to-buy', the correct answer is any multiple of six greater than or
;; equal to 12.  You only need two cases: one to handle the case where you need
;; 6 or fewer (in which case you buy 12) and one other case where you buy the
;; lowest multiple of 6 greater than the number you need.  Even better, you can
;; do 'eggs-to-buy' with just 'max' and 'exact-ceiling' and no 'cond'.

;; === correctness ===

;; problem 1 (cm/in)                 6/ 6
;; problem 1 (molecular-w)           5/ 5
;; problem 1 (eval quad)             5/ 5

;; problem 2 (valid-celsius?)        5/ 5
;; problem 2 (within?)               5/ 5
;; problem 2 (on-quadratic?)         6/ 6

;; problem 3 (eggs-to-buy)           6/ 6

;; problem 4 (g)                     6/ 6

;; === style ===

;; code layout                       6/ 8
;; identifiers are well named        8/ 8
;; program decomposition (helpers)   3/ 4

;; contracts (types)                 8/ 8
;; well-written purposes             8/ 8
;; adequate tests                    8/ 8

;; clarity (clear logic)             6/ 6

;; svn used correctly                6/ 6

;; _total-score_                  97/ 100

;; grader: Nick Seltzer
#lang typed/racket
(require typed/test-engine/racket-tests)


;; ---- Structures and Types ----

(define-struct Point
  ([x : Real]
   [y : Real])
  #:transparent)

(define-type Dataset (Listof Point))

(define-struct Line
  ([m : Real]
   [b : Real])
  #:transparent)


;; ---- Helper Functions for List of Reals ----

(: sum : (Listof Real) -> Real)
;; find the sum of the components of a list of Reals
(define (sum list)
  (cond
    [(empty? list) 0]
    [(cons? list) (+ (first list) (sum (rest list)))]))
;; TESTS
(check-within (sum '(1 2 3 4 5 6 7 8 9 10)) 55 0.001)
(check-within (sum '(1 2 3 4 5 6 7 8 9 10 11.23 4.67)) 70.9 0.001)

(: mult-lists : (Listof Real) (Listof Real) -> (Listof Real))
;; multiply the respective terms of two lists and return a list of those products
(define (mult-lists list1 list2)
  (cond
    [(not (= (length list1) (length list2)))
     (error "lists not same length, can't multiply elements")]
    [(empty? list1) empty]
    [(cons (* (first list1) (first list2))
           (mult-lists (rest list1) (rest list2)))]))
;; TESTS
(check-within (mult-lists '(1 2 3 4) '(1 2 3 4)) '(1 4 9 16) 0.001)
(check-error (mult-lists '(1 2 3 4) '(1 2 3))
             "lists not same length, can't multiply elements")


;; ---- Stock Datasets for Testing ----

(: stockset1 : Dataset)
;; stock Dataset with slope 0.644 and y-intercept 26.781 (approximate)
(define stockset1
  (cons (make-Point 95 85)
  (cons (make-Point 85 95)
  (cons (make-Point 80 70)
  (cons (make-Point 70 65)
  (cons (make-Point 60 70)
        empty))))))

(: stockset2 : Dataset)
;; stock Dataset with slope 0.314 and y-intercept 10.731 (approximate)
(define stockset2
  (cons (make-Point 1 17.69)
  (cons (make-Point 8 19)
  (cons (make-Point 19 0)
  (cons (make-Point 70 0.45)
  (cons (make-Point 69 69)
        empty))))))

;; ---- Helper Functions for Datasets (List of Points) ----

(: get-x-vals : Dataset -> (Listof Real))
;; get the x-values of a list of Points
(define (get-x-vals dataset)
  (match dataset
    ['() empty]
    [(cons (Point x _) tail) (cons x (get-x-vals tail))]))
;; TESTS
(check-within (get-x-vals (list (make-Point 1 2) (make-Point 3 4))) '(1 3) 0.001)
(check-within (get-x-vals (list (make-Point 0 1) (make-Point 3.00 4.343)))
              '(0 3) 0.001)
(check-within (get-x-vals stockset1) '(95 85 80 70 60) 0.001)
(check-within (get-x-vals stockset2) '(1 8 19 70 69) 0.001)

(: get-y-vals : Dataset -> (Listof Real))
;; get the y-values of a list of Points
(define (get-y-vals dataset)
  (match dataset
    ['() empty]
    [(cons (Point _ y) tail) (cons y (get-y-vals tail))]))
;; TESTS
(check-within (get-y-vals (list (make-Point 1 2) (make-Point 3 4))) '(2 4) 0.001)
(check-within (get-y-vals (list (make-Point 0 1) (make-Point 3.00 4.343)))
              '(1 4.343) 0.001)
(check-within (get-y-vals stockset1) '(85 95 70 65 70) 0.001)
(check-within (get-y-vals stockset2) '(17.69 19 0 0.45 69) 0.001)

(: slope : Dataset -> Real)
;; find the best-fit slope for the linear model of a Dataset
(define (slope dataset)
  (local
    {(define n (length dataset))
     (define x-list (get-x-vals dataset))
     (define y-list (get-y-vals dataset))}
    (/ (- (* n (sum (mult-lists x-list
                                y-list)))
          (* (sum x-list) (sum y-list)))
       (- (* n (sum (mult-lists x-list
                                x-list)))
          (sqr (sum x-list))))))
;; TESTS
(check-within (slope stockset1) 0.644 0.001)
(check-within (slope stockset2) 0.314 0.001)
(check-within (slope (list (make-Point 0 0) (make-Point 1 1))) 1 0.001)

(: intercept : Dataset -> Real)
;; find the y-intercept for the linear model of a Dataset
(define (intercept dataset)
  (local
    {(define n (length dataset))
     (define x-list (get-x-vals dataset))
     (define y-list (get-y-vals dataset))}
    (/ (- (* (sum y-list) (sum (mult-lists x-list
                                           x-list)))
          (* (sum x-list) (sum (mult-lists x-list
                                           y-list))))
       (- (* n (sum (mult-lists x-list
                                x-list)))
          (sqr (sum x-list))))))
;; TESTS
(check-within (intercept stockset1) 26.781 0.001)
(check-within (intercept stockset2) 10.731 0.001)
(check-within (intercept (list (make-Point 0 0) (make-Point 1 1))) 0 0.001)


;; ==== Main Function ====

(: linreg : Dataset -> Line)
;; use the generated slope and y-intercept to output the best-fit Line
(define (linreg dataset)
  (cond
    [(< (length dataset) 2)
     (error
      "linear regression analysis isn't meaningful for such a small dataset")]
    [else (make-Line (slope dataset) (intercept dataset))]))
;; TESTS
(check-within (linreg stockset1) (make-Line 0.644 26.781) 0.001)
(check-within (linreg stockset2) (make-Line 0.314 10.731) 0.001)
(check-within (linreg (list (make-Point 0 0) (make-Point 1 1)))
              (make-Line 1 0) 0.001)

(test)

;; evaluation

;; === correctness ===

;; correctness of slope              20/20
;; correctness of intercept          20/20
;; correctness of linreg             4/ 4

;; === style ===

;; code layout                       8/ 8
;; identifiers are well named        8/ 8
;; program organization              4/ 4

;; contracts (types)                 8/ 8
;; well-written purposes             8/ 8
;; adequate tests                    8/ 8

;; clarity (clear logic)             6/ 6

;; svn used correctly                6/ 6

;; _total-score_                   100/ 100

;; graded by Jonathan Jin
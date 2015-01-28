#lang typed/racket
(require typed/test-engine/racket-tests)


;; ==== Problem 1 ====

(: area-of-intersection : Real Real -> Number)
;; find the area of intersection of two circles of the same radius separated
;; by a distance
;; NOTE: A large amount of derivation was done beforehand, and only the 
;; condensed formula arrived at is placed into the definition. I feel there is
;; more merit in leaving out the "derivation by helper functions" that could
;; have taken place (involving subtracting isosceles triangles from fractions 
;; of circles) as in this case it would have been more byzantine. Also I'm
;; pretty sure the formula can be found online. (please don't take off points?)
(define (area-of-intersection radius distance)
  (- (* 2 (sqr radius) (acos (/ distance (* 2 radius))))
     (* distance 
        (sqrt (- (sqr radius) 
                 (sqr (/ distance 2)))))))
; grader: does not account for case where circles are completely separated. -1

;; TESTS
(check-within (area-of-intersection 1 0) pi 0.001)
(check-within (area-of-intersection 3 0) (* 9 pi) 0.001)
(check-within (area-of-intersection 1 2) 0 0.001)
(check-within (area-of-intersection 3 5) 2.251 0.001)


;; ==== Problem 2 ====

(: mobile-broadband-cost : Integer -> Integer)
;; determine the amount of money in dollars charged by Singular Wireless based
;; on data used in megabytes
(define (mobile-broadband-cost megabytes)
  (cond
    [(<= megabytes 300) 20]
    [(<= megabytes 3072) 30]
    [else (+ 30 
             (* 15 (exact-ceiling (/ (- megabytes 3072) 
                                     1024))))]))
;; TESTS
(check-expect (mobile-broadband-cost 150) 20)
(check-expect (mobile-broadband-cost 300) 20)
(check-expect (mobile-broadband-cost 1000) 30)
(check-expect (mobile-broadband-cost 5000) 60)


;; ==== Problem 3 ====

(define-struct Food
  ([name : String]
   [serving : Exact-Rational]
   [fat : Exact-Rational]
   [protein : Exact-Rational]
   [carbs : Exact-Rational])
  #:transparent)

(define-struct Meal
  ([entree : Food]
   [side : Food]
   [dessert : Food])
  #:transparent)

(: food-kcal : Food -> Exact-Rational)
;; give the number of kilocalories of a food based on grams of fat, protein,
;; and carbohydrates
(define (food-kcal food)
  (+ (* 9 (Food-fat food)) (* 4 (Food-protein food)) (* 4 (Food-carbs food))))
;; TESTS
(check-expect (food-kcal (make-Food "ham" 100 4 22 12)) 172)
(check-expect (food-kcal (make-Food "turkey" 100 3 21 11)) 155)

(: foods-fat : Food Food -> Exact-Rational)
;; give the total grams of fat in two foods
(define (foods-fat food1 food2)
  (+ (Food-fat food1) (Food-fat food2)))
;; TESTS
(check-expect (foods-fat (make-Food "ham" 100 4 22 12)
                         (make-Food "turkey" 100 3 21 11)) 7)
(check-expect (foods-fat (make-Food "Big Mac" 1000 500/2 22 12)
                         (make-Food "Double Cheeseburger" 1000 300/17 21 11)) 
              4550/17)

(: food-scale : Food Exact-Rational -> Food)
;; give a new food with scaled serving size, fat, protein and carbs
(define (food-scale food scalar)
  (make-Food (Food-name food)
             (* scalar (Food-serving food))
             (* scalar (Food-fat food))
             (* scalar (Food-protein food))
             (* scalar (Food-carbs food))))
;; TESTS
(check-expect (food-scale (make-Food "ham" 100 4 22 12) 1/2) 
              (make-Food "ham" 50 2 11 6))
(check-expect (food-scale (make-Food "Big Mac" 1000 500/2 22 12) 1/4) 
              (make-Food "Big Mac" 250 125/2 11/2 3))

(: food-daily-pct-kcal : Food Integer -> Exact-Rational)
;; give the percent of kilocalorie quota filled by the given food
(define (food-daily-pct-kcal food quota)
  (/ (food-kcal food) quota))
;; TESTS
(check-expect (food-daily-pct-kcal (make-Food "mystery meat" 1000 200 300 100)
                                   2000) 17/10)
(check-expect (food-daily-pct-kcal (make-Food "mystery meat" 1000 20 30 10)
                                   2000) 17/100)

(: meal-kcal : Meal -> Exact-Rational)
;; give how many kilocalories a meal contains based on previous definitions
(define (meal-kcal meal)
  (+ (food-kcal (Meal-entree meal)) 
     (food-kcal (Meal-side meal)) 
     (food-kcal (Meal-dessert meal))))
;; TESTS
(check-expect (meal-kcal (make-Meal (make-Food "Big Mac" 1000 500/2 22 12) 
                                    (make-Food "mystery meat" 1000 200 300 100) 
                                    (make-Food "key lime pie" 1000 22 33 44)))
              6292)
(check-expect (meal-kcal (make-Meal (make-Food "Big Mac" 1000 645 21 7) 
                                    (make-Food "mystery meat" 1000 300 300 200) 
                                    (make-Food "key lime pie" 1000 12 93 24)))
              11193)

(: meal-healthy? : Meal Integer -> Boolean)
;; give whether a meal is healthy based on some contrived criteria
(define (meal-healthy? meal quota)
  (and (<= (meal-kcal meal) (/ quota 2))
       (<= (meal-fat meal) 50)
       (<= (* (meal-fat meal) 9) (* (meal-kcal meal) 1/3))))
;; TESTS
(check-expect (meal-healthy? (make-Meal (make-Food "Big Mac" 1000 10 22 12) 
                                    (make-Food "mystery meat" 1000 20 300 100) 
                                    (make-Food "key lime pie" 1000 20 33 44)) 
                             20000) #t)
(check-expect (meal-healthy? (make-Meal (make-Food "Big Mac" 1000 10 22 12) 
                                    (make-Food "mystery meat" 1000 20 300 100) 
                                    (make-Food "key lime pie" 1000 21 33 44)) 
                             20000) #f)
(check-expect (meal-healthy? (make-Meal (make-Food "Big Mac" 1000 10 220 120) 
                                    (make-Food "mystery meat" 1000 20 3000 100) 
                                    (make-Food "key lime pie" 1000 20 330 440)) 
                             10000) #f)
(check-expect (meal-healthy? (make-Meal (make-Food "Big Mac" 1000 10 2 1) 
                                    (make-Food "mystery meat" 1000 20 3 1) 
                                    (make-Food "key lime pie" 1000 20 3 4)) 
                             20000) #f)

(: meal-fat : Meal -> Exact-Rational)
;; give how many grams of fat a meal contains
(define (meal-fat meal)
  (+ (Food-fat (Meal-entree meal)) 
     (Food-fat (Meal-side meal)) 
     (Food-fat (Meal-dessert meal))))
;; TESTS
(check-expect (meal-fat (make-Meal (make-Food "Big Mac" 1000 500/2 22 12) 
                                    (make-Food "mystery meat" 1000 200 300 100) 
                                    (make-Food "key lime pie" 1000 22 33 44)))
              472)
(check-expect (meal-fat (make-Meal (make-Food "Big Mac" 1000 645 21 7) 
                                    (make-Food "mystery meat" 1000 300 300 200) 
                                    (make-Food "key lime pie" 1000 12 93 24)))
              957)


;; ==== Problem 4 ====

(define-struct Time
  ([hour : Integer]
   [minute : Integer]
   [second : Integer]
   [zone : Integer])  
  ; zone value designates the UTC zone with no daylight savings
  ; e.g. Chicago is -6
  #:transparent)

(: before? : Time Time -> Boolean)
;; return true if the first time happens before the second, assuming they
;; happen in the same day
(define (before? time1 time2)
  (cond
    [(< (- (Time-hour time2) (- (Time-zone time2) 
                                (Time-zone time1))) (Time-hour time1)) #f]
    [(and (= (- (Time-hour time2) (- (Time-zone time2) 
                                     (Time-zone time1))) (Time-hour time1))
          (< (Time-minute time2) (Time-minute time1))) #f]
    [(and (= (- (Time-hour time2) (- (Time-zone time2) 
                                     (Time-zone time1))) (Time-hour time1))
          (= (Time-minute time2) (Time-minute time1))
          (<= (Time-second time2) (Time-second time1))) #f]
    [else #t]))
; grader: does not properly treat hour/timezone in dayless representation (see Piazza). -3
; grader: unnecessary cond; should just and these cases together. -1 layout
;; TESTS
(check-expect (before? (make-Time 19 20 21 -6) (make-Time 19 20 20 -6)) #f)
(check-expect (before? (make-Time 19 20 20 -6) (make-Time 19 20 21 -6)) #t)
(check-expect (before? (make-Time 19 20 21 -6) (make-Time 17 22 20 -5)) #f)
(check-expect (before? (make-Time 19 20 21 -6) (make-Time 15 20 20 -11)) #t)

(: add-minutes : Time Integer -> Time)
;; produce a time as a result of adding minutes to an input time
(define (add-minutes time mintoadd)
  (make-Time (cond
               [(>= (+ (Time-hour time) (quotient (+ (Time-minute time) mintoadd) 
                                                  60)) 
                    24)
                (- (+ (Time-hour time) (quotient (+ (Time-minute time) mintoadd) 
                                                 60)) 
                   24)]
               [else (+ (Time-hour time) (quotient (+ (Time-minute time) mintoadd) ;; grader: lines over 80 chars. -1 layout
                                                   60))])
             (cond
               [(>= (+ (Time-minute time) (remainder mintoadd 60)) 60)
                (- (+ (Time-minute time) (remainder mintoadd 60)) 60)]
               [else (+ (Time-minute time) (remainder mintoadd 60))])
             (Time-second time)
             (Time-zone time)))
; grader: does not properly treat negative inputs. -1
;; TESTS
(check-expect (add-minutes (make-Time 19 20 21 -6) 100) (make-Time 21 0 21 -6))
(check-expect (add-minutes (make-Time 12 36 21 -6) 100) (make-Time 14 16 21 -6))

(: diff-sec : Time Time -> Integer)
;; calculate the number of seconds between two times
(define (diff-sec time1 time2)
  (abs (+ (* (- (Time-hour time1) 
                (- (Time-hour time2) 
                   (- (Time-zone time2) 
                      (Time-zone time1)))) 3600)
          (* (- (Time-minute time1) (Time-minute time2)) 60)
          (* (- (Time-second time1) (Time-second time2)) 1))))
; grader: does not properly treat hour/timezones in dayless representation. -3
;; TESTS
(check-expect (diff-sec (make-Time 19 20 21 -6) (make-Time 19 20 21 -6)) 0)
(check-expect (diff-sec (make-Time 11 20 21 -6) (make-Time 19 24 26 -6)) 29045)
(check-expect (diff-sec (make-Time 19 24 26 -6) (make-Time 11 20 21 -6)) 29045)
(check-expect (diff-sec (make-Time 19 24 26 -6) (make-Time 11 20 21 -3)) 39845)

(test)

;; evaluation

;; ====== correctness

;; === correctness ===

;; problem 1 (venn)                 4 / 5

;; problem 2 (singular cost)        5 / 5

;; problem 3 (food-cal)             5 / 5
;; problem 3 (foods-fat)            2 / 2
;; problem 3 (food-scale)           2 / 2
;; problem 3 (food-pct-cal)         2 / 2
;; problem 3 (meal-cal)             2 / 2
;; problem 3 (meal-healthy?)        6 / 6

;; problem 4 (before?)              5 / 5
;; problem 4 (add-minutes)          3 / 5
;; problem 4 (diff-sec)             5 / 5

;; === style ===

;; code layout                       6 / 8 (lines over 80 chars, unnecessary cond)
;; identifiers are well named        8 / 8
;; program decomposition (helpers)   4 / 4

;; contracts (types)                 8 / 8
;; well-written purposes             8 / 8
;; adequate tests                    8 / 8

;; clarity (clear logic)             6 / 6

;; svn used correctly                6 / 6

;; _total-score_                    96 / 100

;; grader: Mark Maskeri

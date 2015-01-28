#lang typed/racket
(require typed/test-engine/racket-tests)
(require/typed 2htdp/image
  [#:opaque Image image?]
  [overlay (-> Image Image Image)]
  [empty-image Image]
  [circle (-> Integer String String Image)]
  [square (-> Integer String String Image)]
  [rectangle (-> Integer Integer String String Image)]
  [triangle (-> Integer String String Image)]
  [star (-> Integer String String Image)]
  [radial-star (-> Integer Integer Integer String String Image)])


 ;; ==== Problem 1 ====

(: concentric-circles (-> String Integer Integer Image))
;; consume a color name, a max radius, and a delta of radius decrease at each
;; step and produce concentric circles
(define (concentric-circles color radius delta)
  (cond
    [(<= radius 0) empty-image]
    [else (overlay (concentric-circles color (- radius delta) delta)
                   (circle radius "outline" color))]))
;; VISUAL TEST (uncomment to use)
;(concentric-circles "green" 100 30)
; ^ four concentric circles of radii 100, 70, 40, 10
;(concentric-circles "green" 100 50)
; ^ two concentric circes of radii 100 and 50

(: blue-star (-> Integer Image))
;; [TEST FUNC] create a test function for use in general concentric function by 
;; paring down necessary inputs for creating a specific shape
(define (blue-star radius)
  (star radius "outline" "blue"))
;; [TEST FUNC] VISUAL TEST (uncomment to use)
;(blue-star 100)
; ^ a blue star of radius 100
;(blue-star 50)
; ^ a blue star of radius 50
;(blue-star 0)
; ^ nothing

(: concentric (-> (-> Integer Image) Integer Integer Image))
;; consume a shape-drawing function, a max radius, and a delta of radius decrease
;; at each step and produce concentric shapes
(define (concentric draw-func radius delta)
  (cond
    [(<= radius 0) empty-image]
    [else (overlay (concentric draw-func (- radius delta) delta)
                   (draw-func radius))]))
;; VISUAL TEST (uncomment to use)
;(concentric blue-star 100 30)
; ^ four concentric blue stars of radii 100, 70, 40, 10
;(concentric blue-star 100 50)
; ^ two concentric blue stars of radii 100 and 50


;; ==== Problem 2 ====

(define-struct Food
  ([name : String]
   [serving : Exact-Rational]
   [fat : Exact-Rational]
   [protein : Exact-Rational]
   [carbs : Exact-Rational])
  #:transparent)

(define-type Meal (Listof Food))

; Some test cases for later use
(define cheeseburger (make-Food "cheeseburger" 100 10 15 20))
(define fries (make-Food "fries" 100 30 15 20))
(define tea (make-Food "tea" 100 10 15 20))
(define vegburger (make-Food "vegburger" 100 5 20 30))
(define cheeseburger-meal (list cheeseburger fries tea))
(define cheeseburger-sandwich (list cheeseburger)) ; meal of just a cheeseburger
(define vegburger-sandwich (list vegburger)) ; meal of just a vegburger

(: meal-fat (-> Meal Exact-Rational))
;; consume a Meal and return the total grams of fat in all constituent Foods
(define (meal-fat meal)
  (cond
    [(empty? meal) 0]
    [else (+ (Food-fat (first meal)) (meal-fat (rest meal)))]))
;; TESTS
(check-expect (meal-fat cheeseburger-meal) 50)
(check-expect (meal-fat cheeseburger-sandwich) 10)

(: fatty-food-in-meal? (-> Meal Boolean))
;; consume a Meal and return true iff at least one of the Foods contains more
;; than 25g of fat
(define (fatty-food-in-meal? meal)
  (cond
    [(empty? meal) #f]
    [(> (Food-fat (first meal)) 25) #t]
    [else (fatty-food-in-meal? (rest meal))]))
;; TESTS
(check-expect (fatty-food-in-meal? cheeseburger-meal) #t)
(check-expect (fatty-food-in-meal? cheeseburger-sandwich) #f)

(: eliminate-grease (-> Meal Meal))
;; consume a Meal and return a new Meal that contains only Foods with less than
;; 25g of fat
(define (eliminate-grease meal)
  (cond
    [(empty? meal) empty]
    [(< (Food-fat (first meal)) 25) (cons (first meal)
                                          (eliminate-grease (rest meal)))]
    [else (eliminate-grease (rest meal))]))
;; TESTS
(check-expect (eliminate-grease cheeseburger-meal) (list cheeseburger tea))
(check-expect (eliminate-grease cheeseburger-sandwich) cheeseburger-sandwich)

(: conform-to-diet (-> Meal Meal))
;; consume a Meal and return a new Meal with the same Foods but scaled-down
;; serving sizes so that total fat is less than or equal to 25g
(define (conform-to-diet meal)
  (local
    {(define fat-scalar (/ 25 (meal-fat meal)))}
    (cond
      [(> (meal-fat meal) 25) (meal-scale meal fat-scalar)]
      [else meal])))
;; TESTS
(check-expect (conform-to-diet cheeseburger-meal)
              (list (make-Food "cheeseburger" 50 5 15/2 10)
                    (make-Food "fries" 50 15 15/2 10)
                    (make-Food "tea" 50 5 15/2 10)))
(check-expect (conform-to-diet cheeseburger-sandwich) cheeseburger-sandwich)

(: food-scale : Food Exact-Rational -> Food) ; entirely taken from HW2
;; [AUX 1] give a new food with scaled serving size, fat, protein and carbs
(define (food-scale food scalar)
  (make-Food (Food-name food)
             (* scalar (Food-serving food))
             (* scalar (Food-fat food))
             (* scalar (Food-protein food))
             (* scalar (Food-carbs food))))
;; [AUX 1] TESTS
(check-expect (food-scale (make-Food "ham" 100 4 22 12) 1/2) 
              (make-Food "ham" 50 2 11 6))
(check-expect (food-scale (make-Food "Big Mac" 1000 500/2 22 12) 1/4) 
              (make-Food "Big Mac" 250 125/2 11/2 3))

(: meal-scale (-> Meal Exact-Rational Meal))
;; [AUX 2] recursively use food-scale to scale an entire Meal
(define (meal-scale meal scalar)
  (cond
    [(empty? meal) empty]
    [else (cons (food-scale (first meal) scalar)
                (meal-scale (rest meal) scalar))]))
;; [AUX 2] TESTS
(check-expect (meal-scale cheeseburger-meal 1/2) 
              (list (make-Food "cheeseburger" 50 5 15/2 10)
                    (make-Food "fries" 50 15 15/2 10)
                    (make-Food "tea" 50 5 15/2 10)))
(check-expect (meal-scale cheeseburger-sandwich 1/4)
              (list (make-Food "cheeseburger" 50/2 5/2 15/4 5)))

(: meal-healthy? (-> Meal Integer Boolean))
;; consume a Meal and a daily kilocalorie quota and return true iff the meal
;; - contains less than or equal to 50% of daily kilocalorie quota
;; - contains less than or equal to 50g of fat
;; - has calories from fat value that is less than or equal to 1/3 of the
;;   total calories
(define (meal-healthy? meal quota)
  (and (<= (meal-kcal meal) (/ quota 2))
       (<= (meal-fat meal) 50)
       (<= (* (meal-fat meal) 9) (/ (meal-kcal meal) 3))))
;; TESTS
(check-expect (meal-healthy? cheeseburger-meal 2000) #f)
; ^ first and second conditions satisfied but the third is not
(check-expect (meal-healthy? vegburger-sandwich 2000) #t)

(: food-kcal : Food -> Exact-Rational) ; from HW2
;; [AUX 1] give the number of kilocalories of a food based on grams of fat,
;; protein, and carbohydrates
(define (food-kcal food)
  (+ (* 9 (Food-fat food)) (* 4 (Food-protein food)) (* 4 (Food-carbs food))))
;; [AUX 1] TESTS
(check-expect (food-kcal (make-Food "ham" 100 4 22 12)) 172)
(check-expect (food-kcal (make-Food "turkey" 100 3 21 11)) 155)

(: meal-kcal (-> Meal Exact-Rational))
;; [AUX 2] recursively use food-scale to scale an entire Meal
(define (meal-kcal meal)
  (cond
    [(empty? meal) 0]
    [else (+ (food-kcal (first meal))
             (meal-kcal (rest meal)))]))
;; [AUX 2] TESTS
(check-expect (meal-kcal cheeseburger-meal) 870)
(check-expect (meal-kcal cheeseburger-sandwich) 230)


;; ==== Problem 3 ====

(define-struct Point
  ([x : Real]
   [y : Real])
  #:transparent)

(define-struct Rectangle
  ([length : Real]
   [width : Real]
   [bottom-left : Point])
  #:transparent)

(define-struct Circle
  ([center : Point]
   [radius : Real])
  #:transparent)

; Some test cases for later use
(define stock-circle-A (make-Circle (make-Point 0 0) 1))
(define stock-circle-B (make-Circle (make-Point 0.5 0) 1))
(define stock-circle-C (make-Circle (make-Point 2.5 0) 1))
(define stock-circle-D (make-Circle (make-Point 0.25 0.4330127018922193) 1))
(define stock-circle-E (make-Circle (make-Point 0.25 -0.4330127018922193) 1))
(define stock-circle-F (make-Circle (make-Point 0.5 0) 0.75))
(define stock-circle-G (make-Circle (make-Point 0.25 0) 1.25))

(: throw-dart : Rectangle -> Point)
;; consumes a Rectangle and produces a Point chosen at random within it
(define (throw-dart rectangle)
  (make-Point (+ (Point-x (Rectangle-bottom-left rectangle))
                 (* (Rectangle-length rectangle) (random)))
              (+ (Point-y (Rectangle-bottom-left rectangle))
                 (* (Rectangle-width rectangle) (random)))))
;; TESTS 
;; NOTE: checking with repeats to see if random darts always land in rectangle
(check-within (throw-dart (make-Rectangle 4 3 (make-Point 0 0))) 
              (make-Point 2 1.5) 2)
(check-within (throw-dart (make-Rectangle 4 3 (make-Point 0 0))) 
              (make-Point 2 1.5) 2)
(check-within (throw-dart (make-Rectangle 4 3 (make-Point 0 0))) 
              (make-Point 2 1.5) 2)
(check-within (throw-dart (make-Rectangle 4 3 (make-Point 0 0))) 
              (make-Point 2 1.5) 2)

(: within-circle? : Circle Point -> Boolean)
;; consume a Circle and a Point and return #t iff Point is within the Circle
(define (within-circle? circle point)
  (<= (sqrt (+ (sqr (- (Point-x point) (Point-x (Circle-center circle))))
               (sqr (- (Point-y point) (Point-y (Circle-center circle))))))
      (Circle-radius circle)))
;; TESTS
(check-expect (within-circle? (make-Circle (make-Point 0 0) 1)
                              (make-Point 0.5 0.5)) #t)
(check-expect (within-circle? (make-Circle (make-Point 0 0) 1)
                              (make-Point 0.25 0.75)) #t)

(: bounding-box : (Listof Circle) -> Rectangle)
;; consume a list of Circles and produce the smallest Rectangle that encloses
;; all of the Circles
(define (bounding-box list)
  (make-Rectangle (- (max-list (list-of-largest-x list))
                     (min-list (list-of-smallest-x list)))
                  (- (max-list (list-of-largest-y list))
                     (min-list (list-of-smallest-y list)))
                  (make-Point (min-list (list-of-smallest-x list))
                              (min-list (list-of-smallest-y list)))))
;; TESTS
(check-within (bounding-box (list (make-Circle (make-Point 0 0) 1)
                                  (make-Circle (make-Point 0 0) 1)
                                  (make-Circle (make-Point 0 0) 1)))
              (make-Rectangle 2 2 (make-Point -1 -1)) 0.001)
(check-within (bounding-box (list (make-Circle (make-Point 0 0) 1)
                                  (make-Circle (make-Point 1 0) 1)
                                  (make-Circle (make-Point 2 0) 1)))
              (make-Rectangle 4 2 (make-Point -1 -1)) 0.001)

(: list-of-largest-x (-> (Listof Circle) (Listof Real)))
;; [AUX 1] consume a list of Circles and return a list of largest possible
;; x-values based on centers and radii
(define (list-of-largest-x list)
  (cond
    [(empty? list) empty]
    [else (cons (+ (Point-x (Circle-center (first list))) 
                   (Circle-radius (first list)))
                (list-of-largest-x (rest list)))]))
;; [AUX 1] TESTS
(check-within (list-of-largest-x (list (make-Circle (make-Point 0 0) 3)
                                       (make-Circle (make-Point 1 0) 1)
                                       (make-Circle (make-Point 1 0) 5)))
              '(3 2 6) 0.001)

(: list-of-largest-y (-> (Listof Circle) (Listof Real)))
;; [AUX 2] consume a list of Circles and return a list of largest possible
;; y-values based on centers and radii
(define (list-of-largest-y list)
  (cond
    [(empty? list) empty]
    [else (cons (+ (Point-y (Circle-center (first list))) 
                   (Circle-radius (first list)))
                (list-of-largest-y (rest list)))]))
;; [AUX 2] TESTS
(check-within (list-of-largest-y (list (make-Circle (make-Point 0 0) 3)
                                       (make-Circle (make-Point 1 0) 1)
                                       (make-Circle (make-Point 1 0) 5)))
              '(3 1 5) 0.001)

(: list-of-smallest-x (-> (Listof Circle) (Listof Real)))
;; [AUX 3] consume a list of Circles and return a list of smallest possible
;; x-values based on centers and radii
(define (list-of-smallest-x list)
  (cond
    [(empty? list) empty]
    [else (cons (- (Point-x (Circle-center (first list))) 
                   (Circle-radius (first list)))
                (list-of-smallest-x (rest list)))]))
;; [AUX 3] TESTS
(check-within (list-of-smallest-x (list (make-Circle (make-Point 0 0) 3)
                                        (make-Circle (make-Point 1 0) 1)
                                        (make-Circle (make-Point 1 0) 5)))
              '(-3 0 -4) 0.001)

(: list-of-smallest-y (-> (Listof Circle) (Listof Real)))
;; [AUX 4] consume a list of Circles and return a list of smallest possible
;; y-values based on centers and radii
(define (list-of-smallest-y list)
  (cond
    [(empty? list) empty]
    [else (cons (- (Point-y (Circle-center (first list))) 
                   (Circle-radius (first list)))
                (list-of-smallest-y (rest list)))]))
;; [AUX 4] TESTS
(check-within (list-of-smallest-y (list (make-Circle (make-Point 0 0) 3)
                                        (make-Circle (make-Point 1 0) 1)
                                        (make-Circle (make-Point 1 0) 5)))
              '(-3 -1 -5) 0.001)

(: max-list : (Listof Real) -> Real)
;; [AUX 5] consume a list of Reals and return the largest Real
(define (max-list list)
  (cond
    [(= (length list) 1) (first list)]
    [(empty? list) 0]  ; returning 0 here since it makes sense as a side length
    [else (max (first list) (max-list (rest list)))]))
;; [AUX 5] TESTS
(check-within (max-list '(1 2 3 4 5)) 5 0.001)

(: min-list : (Listof Real) -> Real)
;; [AUX 6] consume a list of Reals and return the smallest Real
(define (min-list list)
  (cond
    [(= (length list) 1) (first list)]
    [(empty? list) 0]  ; returning 0 here since it makes sense as a side length
    [else (min (first list) (min-list (rest list)))]))
;; [AUX 6] TESTS
(check-within (min-list '(1 2 3 4 5)) 1 0.001)

(: area-of-intersection-monte-carlo : (Listof Circle) Integer -> Real)
;; consume a list of Circles and a number of trials and return an approximate
;; area of intersection of the Circles
(define (area-of-intersection-monte-carlo list trials)
  (* (/ (number-of-hits list trials) trials)
     (* (Rectangle-length (bounding-box list))
        (Rectangle-width (bounding-box list)))))
;; TESTS
(check-within (area-of-intersection-monte-carlo
               (list (make-Circle (make-Point 0 0) 1) 
                     (make-Circle (make-Point 2 0) 1))
               1000000) 0 0.001)
(check-within (area-of-intersection-monte-carlo
               (list (make-Circle (make-Point 0 0) 1) 
                     (make-Circle (make-Point 0 0) 1))
               1000000) pi 0.2)
(check-within (area-of-intersection-monte-carlo
               (list) 1000000) 0 0.001)
(check-within (area-of-intersection-monte-carlo
               (list stock-circle-A) 1000000) 3.141996 0.01)
(check-within (area-of-intersection-monte-carlo
               (list stock-circle-A stock-circle-B) 1000000) 2.152585 0.01)
(check-within (area-of-intersection-monte-carlo
               (list stock-circle-A stock-circle-B stock-circle-C) 1000000)
              0 0.001)
(check-within (area-of-intersection-monte-carlo
               (list stock-circle-A stock-circle-B stock-circle-D) 1000000)
              1.763094819489706 0.01)
(check-within (area-of-intersection-monte-carlo
               (list stock-circle-A stock-circle-B stock-circle-D stock-circle-E)
               1000000) 1.3856659622217002 0.01)
(check-within (area-of-intersection-monte-carlo
               (list stock-circle-A stock-circle-F) 1000000)
              1.4765130 0.01)
(check-within (area-of-intersection-monte-carlo
               (list stock-circle-A stock-circle-B stock-circle-G) 1000000)
              2.1557875 0.01)

(: within-circle-list? : (Listof Circle) Point -> Boolean)
;; [AUX 1] consume a list of Circles and a Point and return #t iff Point is
;; within every Circle in the list
(define (within-circle-list? list point)
  (cond
    [(empty? list) #t]
    [(and (within-circle? (first list) point)
          (within-circle-list? (rest list) point)) #t]
    [else #f]))
;; [AUX 1] TESTS
(check-expect (within-circle-list? (list) (make-Point 0 0)) #t)
(check-expect (within-circle-list? (list stock-circle-A) (make-Point 1 1)) #f)

(: number-of-hits : (Listof Circle) Integer -> Integer)
;; [AUX 2] consume a list of Circles and a number of trials and return the
;; number of times a random point falls inside the intersection between
;; the Circles
(define (number-of-hits list trials)
  (local
    {(define point (throw-dart (bounding-box list)))}
    (cond
      [(= trials 0) 0]
      [(within-circle-list? list point)
       (+ 1 (number-of-hits list (- trials 1)))]
      [else (number-of-hits list (- trials 1))])))
;; [AUX 2] TESTS
(check-expect (number-of-hits (list (make-Circle (make-Point 0 0) 1) 
                                    (make-Circle (make-Point 2 0) 1)) 1000) 0)
(check-within (number-of-hits (list (make-Circle (make-Point 0 0) 1) 
                                    (make-Circle (make-Point 0 0) 1))
                              1000) 750 150)


;; ==== Problem 4 ====

(define-struct (T) Rose-Tree
  ([value : T]
   [kids  : (Listof (Rose-Tree T))])
  #:transparent)

; ---- Some test cases for later use ----
(: int-tree : (Rose-Tree Integer))
(define int-tree
  (make-Rose-Tree
   4
   (list (make-Rose-Tree -1 empty)
         (make-Rose-Tree 12 empty)
         (make-Rose-Tree 42 empty))))

(: string-tree : (Rose-Tree String))
(define string-tree
  (make-Rose-Tree 
   "AAA"
   (list (make-Rose-Tree "B" 
                         (list (make-Rose-Tree "CC" empty)
                               (make-Rose-Tree "D" empty)))
         (make-Rose-Tree "E"
                         (list (make-Rose-Tree "FFF" empty)))
         (make-Rose-Tree "G" empty))))
; ---------------------------------------

(: rose-size : (All (T) (Rose-Tree T) -> Natural))
;; count the number of nodes in a tree
(define (rose-size tree)
  (local
    {(: rose-sizes : (Listof (Rose-Tree T)) -> Natural)
     ;; loop rose-size around each kid of a node
     (define (rose-sizes list)
       (cond
         [(empty? list) 1]
         [else (+ (rose-size (first list)) (rose-sizes (rest list)))]))}
    (cond
      [(empty? (Rose-Tree-kids tree)) 1]
      [else (rose-sizes (Rose-Tree-kids tree))])))
;; TESTS
(check-expect (rose-size int-tree) 4)
(check-expect (rose-size string-tree) 7)

(: rose-height : (All (T) (Rose-Tree T) -> Natural))
;; determine the number of nodes on the longest path from root to leaf
(define (rose-height tree)
  (cond
    [(empty? (Rose-Tree-kids tree)) 1]
    [else (+ 1 (rose-height (first (Rose-Tree-kids tree))))]))
;; TESTS
(check-expect (rose-height int-tree) 2)
(check-expect (rose-height string-tree) 3)

(test)
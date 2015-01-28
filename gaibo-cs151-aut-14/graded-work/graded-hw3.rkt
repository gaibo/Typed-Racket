#lang typed/racket
(require typed/test-engine/racket-tests)


;; ==== Problem 1 ====

;; Postponed


;; ==== Problem 2 ====

(define-type Nat (U 'Zero Succ))
(define-struct Succ
  ([prev : Nat])
  #:transparent)

(: nat->int : Nat -> Integer)
;; consume a Nat and produce an Integer of the same value
(define (nat->int nat)
  (match nat
    ['Zero 0]
    [(Succ nat-prev) (+ 1 (nat->int nat-prev))]))
;; TESTS
(check-expect (nat->int 'Zero) 0)
(check-expect (nat->int (make-Succ (make-Succ (make-Succ 'Zero)))) 3)

(: int->nat : Integer -> Nat)
;; consume a non-negative Integer and produce a Nat representing the same value
(define (int->nat integer)
  (match integer
    [0 'Zero]
    [_ (Succ (int->nat (- integer 1)))]))
;; TESTS
(check-expect (int->nat 0) 'Zero)
(check-expect (int->nat 3) (Succ (Succ (Succ 'Zero))))

(: nat+ : Nat Nat -> Nat)
;; consume two Nats and produce a Nat representing their sum
(define (nat+ nat1 nat2)
  (match nat2
    ['Zero nat1]
    [(Succ nat2-prev) (Succ (nat+ nat1 nat2-prev))]))
;; TESTS
(check-expect (nat+ (Succ (Succ 'Zero)) 'Zero) (Succ (Succ 'Zero)))
(check-expect (nat+ (Succ (Succ 'Zero)) (Succ (Succ (Succ 'Zero)))) 
              (Succ (Succ (Succ (Succ (Succ 'Zero))))))

(: nat- : Nat Nat -> Nat)
;; consume two Nats and produce a Nat representing their difference
;; NOTE: 'Zero minus and other Nat is 'Zero by definition (no negative rules)
(define (nat- nat1 nat2)
  (match (cons nat1 nat2)
    [(cons 'Zero _) 'Zero]
    [(cons _ 'Zero) nat1]
    [(cons (Succ nat1-prev) (Succ nat2-prev)) (nat- nat1-prev nat2-prev)]))
;; TESTS
(check-expect (nat- 'Zero 'Zero) 'Zero)
(check-expect (nat- 'Zero (Succ (Succ 'Zero))) 'Zero)
(check-expect (nat- (Succ (Succ 'Zero)) 'Zero) (Succ (Succ 'Zero)))
(check-expect (nat- (Succ (Succ (Succ (Succ 'Zero)))) (Succ (Succ 'Zero))) 
              (Succ (Succ 'Zero)))

(: nat< : Nat Nat -> Boolean)
;; consume two Nats and produce #t iff the first is strictly less than second
(define (nat< nat1 nat2)
  (not (equal? (nat- nat2 nat1) 'Zero)))
;; TESTS
(check-expect (nat< (Succ (Succ 'Zero)) (Succ 'Zero)) #f)
(check-expect (nat< (Succ 'Zero) (Succ 'Zero)) #f)
(check-expect (nat< (Succ (Succ 'Zero)) (Succ (Succ (Succ 'Zero)))) #t)
(check-expect (nat< 'Zero (Succ 'Zero)) #t)

(: nat* : Nat Nat -> Nat)
;; consume two Nats and produce a Nat representing their product
(define (nat* nat1 nat2)
  (match nat2
    [(Succ 'Zero) nat1]
    ['Zero 'Zero]
    [(Succ nat2-prev) (nat* (nat+ nat1 nat1) nat2-prev)]))
;;grader: the problem with this is that your third conditional leads to exponential growth
;;grader: in your multiplication. say you do (nat* five six). your code will see that six 
;;grader: is not zero or one and thus will return (nat* ten five), then (nat* twenty four),
;;grader: then (nat* forty three), then (nat* eighty two), then (nat* one-hundred-sixty one),
;;grader: and finally one-hundred-sixty; what this is doing really is nat1*(2^(nat2 - 1))
;;grader: instead of nat1*nat2
;; TESTS
(check-expect (nat* (Succ (Succ 'Zero)) 'Zero) 'Zero)
(check-expect (nat* (Succ (Succ 'Zero)) (Succ (Succ 'Zero)))
              (Succ (Succ (Succ (Succ 'Zero)))))

(: nat/ : Nat Nat -> Nat)
;; consume two Nats and produce a Nat representing their integer quotient
(define (nat/ nat1 nat2)
  (cond
    [(equal? nat1 nat2) (Succ 'Zero)]
    [(equal? (nat- nat1 nat2) 'Zero) 'Zero] 
    [else (Succ (nat/ (nat- nat1 nat2) nat2))]))
;; TESTS
(check-expect (nat/ 'Zero (Succ (Succ 'Zero))) 'Zero)
(check-expect (nat/ (Succ (Succ (Succ (Succ 'Zero)))) (Succ (Succ 'Zero)))
              (Succ (Succ 'Zero)))
(check-expect (nat/ (Succ (Succ 'Zero)) (Succ (Succ (Succ 'Zero))))
              'Zero)
(check-expect (nat/ (Succ (Succ (Succ 'Zero))) (Succ (Succ 'Zero)))
              (Succ 'Zero))

(: nat-remainder : Nat Nat -> Nat)
;; consume two Nats and produce a Nat representing the remainder of their
;; integer division
(define (nat-remainder nat1 nat2)
  (cond
    [(nat< nat1 nat2) nat1]
    [(equal? nat1 nat2) 'Zero]
    [else (nat-remainder (nat- nat1 nat2) nat2)]))
;; TESTS
(check-expect (nat-remainder (Succ (Succ (Succ (Succ 'Zero)))) 
                             (Succ (Succ (Succ (Succ 'Zero)))))
              'Zero)
(check-expect (nat-remainder 'Zero (Succ (Succ (Succ (Succ 'Zero))))) 'Zero)
(check-expect (nat-remainder (Succ (Succ (Succ (Succ (Succ 'Zero))))) 
                             (Succ (Succ 'Zero)))
              (Succ 'Zero))
(check-expect (nat-remainder (Succ (Succ (Succ (Succ 'Zero)))) 
                             (Succ (Succ 'Zero)))
              'Zero)


;; ==== Problem 3 ====

(define-type Point2D (U Cartesian Complex Polar))

(define-struct Cartesian
  ([x : Real]
   [y : Real])
  #:transparent)

(define-struct Polar
  ([radius : Real]
   [angle : Real])
  #:transparent)

(: p2d-from-x-y : Real Real -> Point2D)
;; consume x and y coordinates to produce a Point2D that represents this
;; Cartesian location
(define (p2d-from-x-y x y)
  (make-Cartesian x y))
;; TESTS
(check-within (p2d-from-x-y 3 4) (make-Cartesian 3 4) 0.001)
(check-within (p2d-from-x-y -3.5 12.32) (make-Cartesian -3.5 12.32) 0.001)

(: p2d-from-complex : Complex -> Point2D)
;; consume a complex number and produce a Point2D that represents this
;; complex location
(define (p2d-from-complex n)
  (make-rectangular (real-part n) (imag-part n)))
;; TESTS
(check-expect (p2d-from-complex 1+4i) 1+4i)
(check-expect (p2d-from-complex 4-0i) 4-0i)

(: p2d-from-polar : Real Real -> Point2D)
;; consume a radius and angle and produce a Point2D that represents this
;; polar location
(define (p2d-from-polar radius angle)
  (make-Polar radius angle))
;; TESTS
(check-within (p2d-from-polar 2 pi) (make-Polar 2 pi) 0.001)
(check-within (p2d-from-polar 4 (* 2 pi)) (make-Polar 4 (* 2 pi)) 0.001)

(: p2d-as-cartesian : Point2D -> Point2D)
;; consume a Point2D and produce a Point2D that represents the same location
;; using the Cartesian coordinate representation
(define (p2d-as-cartesian point)
  (cond
    [(complex? point) (make-Cartesian (real-part point) (imag-part point))]
    [(Polar? point) (make-Cartesian (* (Polar-radius point) 
                                       (cos (Polar-angle point)))
                                    (* (Polar-radius point) 
                                       (sin (Polar-angle point))))]
    [else (make-Cartesian (Cartesian-x point) (Cartesian-y point))]))
;; TESTS
(check-within (p2d-as-cartesian (p2d-from-complex 2+3.5i)) 
              (make-Cartesian 2 3.5) 0.001)
(check-within (p2d-as-cartesian (p2d-from-polar 2 pi)) 
              (make-Cartesian -2 0) 0.001)
(check-within (p2d-as-cartesian (p2d-from-x-y 2 3)) 
              (make-Cartesian 2 3) 0.001)
(check-within (p2d-as-cartesian (p2d-from-complex 0)) 
              (make-Cartesian 0 0) 0.001)

(: p2d-as-complex : Point2D -> Point2D)
;; consume a Point2D and produce a Point2D that represents the same location
;; using the complex coordinate representation
(define (p2d-as-complex point)
  (if (complex? point) point 
                      (match point
                        [(Polar radius angle) (make-polar radius angle)]  ; built-in
                        [(Cartesian x y) (make-rectangular x y)])))
;; TESTS
(check-within (p2d-as-complex (p2d-from-complex 2+3.5i)) 2+3.5i 0.001)
(check-within (p2d-as-complex (p2d-from-polar 2 pi)) -2 0.001)
(check-within (p2d-as-complex (p2d-from-x-y 2 3)) 2+3i 0.001)
(check-within (p2d-as-complex (p2d-from-complex 0)) 0 0.001)

(: p2d-as-polar : Point2D -> Point2D)
;; consume a Point2D and produce a Point2D that represents the same location
;; using the polar coordinate representation
(define (p2d-as-polar point)
  (if (complex? point) (make-Polar (magnitude point) (angle point))  ; built-in
                       (match point
                         [(Polar radius angle) point]
                         [(Cartesian x y) (make-Polar (sqrt (+ (sqr x) (sqr y)))
                                                      (atan (/ y x)))])))
;; TESTS
(check-within (p2d-as-polar (p2d-from-complex 0+3.5i)) 
              (make-Polar 3.5 (/ pi 2)) 0.001)
(check-within (p2d-as-polar (p2d-from-polar 2 pi)) (make-Polar 2 pi) 0.001)
(check-within (p2d-as-polar (p2d-from-x-y 3.75 0)) (make-Polar 3.75 0) 0.001)
(check-within (p2d-as-polar (p2d-from-complex 1)) (make-Polar 1 0) 0.001)

(: p2d-x : Point2D -> Real)
;; consume a Point2D and produce the x value of the point in the 
;; Cartesian coordinate system
(define (p2d-x point)
  (if (complex? point) (real-part point)
                       (match point
                         [(Polar radius angle) (* radius (cos angle))]
                         [(Cartesian x y) x])))
;; TESTS
(check-within (p2d-x (p2d-from-complex 2+3.5i)) 2 0.001)
(check-within (p2d-x (p2d-from-polar 2.5 pi)) -2.5 0.001)
(check-within (p2d-x (p2d-from-x-y 2 3)) 2 0.001)
(check-within (p2d-x (p2d-from-complex 0)) 0 0.001)

(: p2d-y : Point2D -> Real)
;; consume a Point2D and produce the y value of the point in the 
;; Cartesian coordinate system
(define (p2d-y point)
  (if (complex? point) (imag-part point)
                       (match point
                         [(Polar radius angle) (* radius (sin angle))]
                         [(Cartesian x y) y])))
;; TESTS
(check-within (p2d-y (p2d-from-complex 2+3.5i)) 3.5 0.001)
(check-within (p2d-y (p2d-from-polar 2.5 pi)) 0 0.001)
(check-within (p2d-y (p2d-from-x-y 2 3)) 3 0.001)
(check-within (p2d-y (p2d-from-complex 0)) 0 0.001)

(: p2d-n : Point2D -> Number)
;; consume a Point2D and produce a complex number that represents the
;; point's location
(define (p2d-n point)
  (make-rectangular (p2d-x point) (p2d-y point)))
;; TESTS
(check-within (p2d-n (p2d-from-complex 2+3.5i)) 2+3.5i 0.001)
(check-within (p2d-n (p2d-from-polar 2.5 pi)) -2.5 0.001)
(check-within (p2d-n (p2d-from-x-y 2 3)) 2+3i 0.001)
(check-within (p2d-n (p2d-from-complex 0)) 0 0.001)

(: p2d-r : Point2D -> Real)
;; consume a Point2D and produce the radius of that point as represented in the
;; polar coordinate system
(define (p2d-r point)
  (magnitude (p2d-n point)))
;; TESTS
(check-within (p2d-r (p2d-from-complex 2+3.5i)) 4.031 0.001)
(check-within (p2d-r (p2d-from-polar 2.5 pi)) 2.5 0.001)
(check-within (p2d-r (p2d-from-x-y 2 3)) 3.606 0.001)
(check-within (p2d-r (p2d-from-complex 0)) 0 0.001)

(: p2d-theta : Point2D -> Real)
;; consume a Point2D and produce the angle of that point as represented in the
;; polar coordinate system
(define (p2d-theta point)
  (angle (p2d-n point)))
;; TESTS
(check-within (p2d-theta (p2d-from-complex 2+3.5i)) 1.052 0.001)
(check-within (p2d-theta (p2d-from-polar 2.5 pi)) pi 0.001)
(check-within (p2d-theta (p2d-from-x-y 2 3)) 0.983 0.001)
(check-within (p2d-theta (p2d-from-complex 1)) 0 0.001)

(: p2d-dist : Point2D Point2D -> Real)
;; consume two Point2Ds and produce the distance between them
(define (p2d-dist point1 point2)
  (sqrt (+ (sqr (- (p2d-x point1) (p2d-x point2))) 
           (sqr (- (p2d-y point1) (p2d-y point2))))))
;; TESTS
(check-within (p2d-dist (p2d-from-complex 2+3i) (p2d-from-x-y 2 3)) 0 0.001)
(check-within (p2d-dist (p2d-from-polar 2.5 pi) (p2d-from-complex 1)) 3.5 0.001)


;; ==== Problem 4 ====

(define-struct Point
  ([x : Real]
   [y : Real])
  #:transparent)

(define-struct Rectangle
  ([length : Real]  ; horizontal
   [width : Real]  ; vertical
   [bottom-left : Point])
  #:transparent)

(define-struct Circle
  ([center : Point]
   [radius : Real])
  #:transparent)

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

(: bounding-box : Circle Circle -> Rectangle)
;; consume two Circles and produce the smallest Rectangle that encloses both
(define (bounding-box circle1 circle2)
  (make-Rectangle 
    (- (max (+ (Point-x (Circle-center circle1)) (Circle-radius circle1)) 
            (+ (Point-x (Circle-center circle2)) (Circle-radius circle2)))
       (min (- (Point-x (Circle-center circle1)) (Circle-radius circle1)) 
            (- (Point-x (Circle-center circle2)) (Circle-radius circle2))))
    (- (max (+ (Point-y (Circle-center circle1)) (Circle-radius circle1)) 
            (+ (Point-y (Circle-center circle2)) (Circle-radius circle2)))
       (min (- (Point-y (Circle-center circle1)) (Circle-radius circle1)) 
            (- (Point-y (Circle-center circle2)) (Circle-radius circle2))))
    (make-Point (min (- (Point-x (Circle-center circle1)) (Circle-radius circle1)) 
                     (- (Point-x (Circle-center circle2)) (Circle-radius circle2)))
                (min (- (Point-y (Circle-center circle1)) (Circle-radius circle1)) 
                     (- (Point-y (Circle-center circle2)) (Circle-radius circle2))))))
;; TESTS
(check-within (bounding-box (make-Circle (make-Point 0 0) 1)
                            (make-Circle (make-Point 1 0) 1))
              (make-Rectangle 3 2 (make-Point -1 -1)) 0.001)

;(: area-of-intersection-monte-carlo : Circle Circle Natural -> Real)
;; consume two Circles and a number of trials and return approximate area of
;; intersection of the Circles
;(define (area-of-intersection-monte-carlo circle1 circle2 trials)
;  (local (define point (throw-dart (bounding-box circle1 circle2))))
;  (match trials
;    [0 0]
;    [_ (if (and (within-circle? circle1 point)
;                (within-circle? circle2 point)) 
;           (+ 1 (area-of-intersection-monte-carlo circle1 circle2 trials))
;           (area-of-intersection-monte-carlo circle1 circle2 trials))

(test)

;; ===== Grader Tests ===== ;;

;; === Problem Two

(define Zerot 'Zero)
(define Onet (Succ Zerot))
(define Twotst (Succ Onet))
(define Threet (Succ Twotst))
(define Fourt (Succ Threet))

(check-expect (nat->int Zerot) 0)
(check-expect (nat->int Onet) 1)
(check-expect (nat->int Twotst) 2)
(check-expect (nat->int Threet) 3)
(check-expect (nat->int Fourt) 4)

(check-expect (int->nat 0) Zerot)
(check-expect (int->nat 1) Onet)
(check-expect (int->nat 2) Twotst)
(check-expect (int->nat 3) Threet)
(check-expect (int->nat 4) Fourt)

(check-expect (nat->int (int->nat 1000)) 1000)

(check-expect (nat+ (int->nat 5) (int->nat 10)) (int->nat 15))
(check-expect (nat+ Fourt Zerot) Fourt)
(check-expect (nat+ Zerot Fourt) Fourt)

(check-expect (nat- (int->nat 10) (int->nat 3)) (int->nat 7))
(check-expect (nat- Fourt Zerot) Fourt)
(check-expect (nat- Zerot Fourt) Zerot)

(check-expect (nat< Threet Fourt) #t)
(check-expect (nat< Fourt Threet) #f)
(check-expect (nat< Threet Threet) #f)

;(check-expect (nat* Threet Zerot) Zerot)
;(check-expect (nat* Zerot Threet) Zerot)
;(check-expect (nat* Threet Onet) Threet)
;(check-expect (nat* Onet Threet) Threet)
;(check-expect (nat* (int->nat 10) (int->nat 15) ) (int->nat 150))

(check-expect (nat/ (int->nat 10) Threet) Threet)
(check-expect (nat/ (int->nat 8) Twotst) Fourt)
(check-expect (nat/ Onet Onet) Onet)
(check-expect (nat/ Onet Twotst) Zerot)
(check-expect (nat/ Fourt Onet) Fourt)

(check-expect (nat-remainder (int->nat 10) Fourt) Twotst)
(check-expect (nat-remainder (int->nat 8) (int->nat 5)) Threet)
(check-expect (nat-remainder Onet Onet) Zerot)
(check-expect (nat-remainder Onet Twotst) Onet)
(check-expect (nat-remainder Fourt Onet) Zerot)

;; === Problem Three

(define origxy (Cartesian 0 0))
(define origrt (Polar 0 0))
(define orign 0)
(define unit0xy (Cartesian 1 0))
(define unit0rt (Polar 1 0))
(define unit0n 1)
(define unitp2xy (Cartesian 0 1))
(define unitp2rt (Polar 1 (/ pi 2)))
(define unitp2n 0+i)
(define unitpxy (Cartesian -1 0))
(define unitprt (Polar 1 pi))
(define unitpn -1)
(define unit3p2xy (Cartesian 0 -1))
(define unit3p2rt (Polar 1 (/ (* 3 pi) 2)))
(define unit3p2n 0-i)

(check-within (p2d-from-x-y 0 0) origxy .0000000001)
(check-within (p2d-from-x-y 0 1) unitp2xy .0000000001)

(check-within (p2d-from-complex orign) orign .0000000001)
(check-within (p2d-from-complex unit3p2n) unit3p2n .0000000001)

(check-within (p2d-from-polar 0 0) origrt .0000000001)
(check-within (p2d-from-polar 1 pi) unitprt .0000000001)

(check-within (p2d-as-cartesian unitpxy) unitpxy .0000000001)
(check-within (p2d-as-cartesian unitprt) unitpxy .0000000001)
(check-within (p2d-as-cartesian unitpn) unitpxy .0000000001)

(check-within (p2d-as-complex unit3p2xy) unit3p2n .0000000001)
(check-within (p2d-as-complex unit3p2rt) unit3p2n .0000000001)
(check-within (p2d-as-complex unit3p2n) unit3p2n .0000000001)

;(check-within (p2d-as-polar unitp2xy) unitp2rt .0000000001)
;(check-within (p2d-as-polar unitp2rt) unitp2rt .0000000001)
;(check-within (p2d-as-polar unitp2n) unitp2rt .0000000001)

(check-within (p2d-x unitpxy) -1 .0000000001)
(check-within (p2d-x unitprt) -1 .0000000001)
(check-within (p2d-x unitpn) -1 .0000000001)

(check-within (p2d-y unitpxy) 0 .0000000001)
(check-within (p2d-y unitprt) 0 .0000000001)
(check-within (p2d-y unitpn) 0 .0000000001)

(check-within (p2d-n unit3p2xy) unit3p2n .0000000001)
(check-within (p2d-n unit3p2rt) unit3p2n .0000000001)
(check-within (p2d-n unit3p2n) unit3p2n .0000000001)

(check-within (p2d-r unitp2xy) 1 .0000000001)
(check-within (p2d-r unitp2rt) 1 .0000000001)
(check-within (p2d-r unitp2n) 1 .0000000001)

(check-within (p2d-theta unitp2xy) (/ pi 2) .0000000001)
(check-within (p2d-theta unitp2rt) (/ pi 2) .0000000001)
(check-within (p2d-theta unitp2n) (/ pi 2) .0000000001)

(check-within (p2d-dist unit3p2xy unitprt) (sqrt 2) .0000000001)
(check-within (p2d-dist unitprt unitpn) 0 .0000000001)
(check-within (p2d-dist unitp2n unit3p2n) 2 .0000000001)

;; === Problem Four

;(check-within (venn-monte-carlo 10 0 1000000) (* 100 pi) 1)
;(check-within (venn-monte-carlo 15 60 1000000) 0 1)

(test)

;; evaluation

;; ====== correctness

;; === correctness ===

;; problem 1 removed from assignment

;; problem 2 (nat->int)             1/ 1
;; problem 2 (int->nat)             1/ 1
;; problem 2 (nat+)                 1/ 1
;; problem 2 (nat-)                 1/ 1
;; problem 2 (nat<)                 2/ 2
;; problem 2 (nat*)                 1/ 3 (see comment under function)
;; problem 2 (nat/)                 2/ 2
;; problem 2 (nat-remainder)        1/ 1

;; problem 3 (p2d-from-x-y)         1/ 1
;; problem 3 (p2d-from-complex)     1/ 1
;; problem 3 (p2d-from-r-theta)     1/ 1
;; problem 3 (p2d-as-cartesian)     2/ 2
;; problem 3 (p2d-as-complex)       2/ 2
;; problem 3 (p2d-as-polar)       1.5/ 2 (because you use arctan(y/x) to convert cartesian to polar, you need a special case for when x is 0 to avoid divide by 0 errors)
;; problem 3 (p2d-x)                2/ 2
;; problem 3 (p2d-y)                1/ 1
;; problem 3 (p2d-n)                2/ 2
;; problem 3 (p2d-r)                2/ 2
;; problem 3 (p2d-theta)            2/ 2
;; problem 3 (p2d-dist)             2/ 2

;; problem 4 (throw-dart)           2/ 2
;; problem 4 (within-circle?)       2/ 2
;; problem 4 (bounding-box)         2/ 2
;; problem 4 (area-of-intersection) 0/ 4 (not implemented to the point it can be run)
;; problem 4 ({left,right}-circle)  0/ 1 (not implemented)
;; problem 4 (venn-monte-carlo)     0/ 1 (not implemented)

;; === style ===

;; code layout                       8/ 8
;; identifiers are well named        8/ 8
;; program decomposition (helpers)   4/ 4

;; contracts (types)                 8/ 8
;; well-written purposes             8/ 8
;; adequate tests                    8/ 8

;; clarity (clear logic)             6/ 6

;; svn used correctly                6/ 6

;; _total-score_                  91.5/ 100

;; grader: Abraham Secular
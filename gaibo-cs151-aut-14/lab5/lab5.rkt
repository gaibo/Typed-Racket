#lang typed/racket
(require typed/test-engine/racket-tests)

;; Gaibo Zhang
;; Lab 5
;; CS151 Autumn 2014, University of Chicago

;; === data definitions ===

(define-type State 
  (U 'AL 'AK 'AZ 'AR 'CA 'CO 'CT 'DE 'DC 'FL 'GA 'HI 'ID 'IL 'IN 'IA 'KS
     'KY 'LA 'ME 'MD 'MA 'MI 'MN 'MS 'MO 'MT 'NE 'NV 'NH 'NJ 'NM 'NY 'NC
     'ND 'OH 'OK 'OR 'PA 'RI 'SC 'SD 'TN 'TX 'UT 'VT 'VA 'WA 'WV 'WI 'WY))
;; 51 state-like entities -- includes Washington, DC

(define-type Party
  (U 'D 'R)) ;; Democrats, Republicans
             ;; apologies to third parties! they're not represented.

(define-struct EV
  ([s  : State]    ;; a state symbol
   [ev : Integer]) ;; electoral votes per this state
  #:transparent)

(define-struct DemProb
  ([s    : State]
   [demp : Real])   ;; probability of Democratic victory on [0.0,1.0] 
  #:transparent)

(define-struct StateResult
  ([s  : State]
   [p  : Party]     ;; winning party
   [ev : Integer])  ;; number of electoral votes for victor
  #:transparent)

(define-struct USAResult
  ([dems : (Listof StateResult)]  ;; states won by Democrats
   [reps : (Listof StateResult)]) ;; states won by Republicans
  #:transparent)

(define-struct Tally
  ([demv : Integer]  ;; simulations where D candidate wins
   [repv : Integer]  ;; simulations where R candidate wins
   [ties : Integer]) ;; simulations where candidates tie
  #:transparent)

(define-type Outcome (U Party 'tie))

;;; === data === 

(: ev-map (Listof EV))
(define ev-map 
  (list
   (EV 'AL 9)
   (EV 'AK 3)
   (EV 'AZ 11)
   (EV 'AR 6)
   (EV 'CA 55)
   (EV 'CO 9)
   (EV 'CT 7)
   (EV 'DE 3)
   (EV 'DC 3)
   (EV 'FL 29)
   (EV 'GA 16)
   (EV 'HI 4)
   (EV 'ID 4)
   (EV 'IL 20)
   (EV 'IN 11)
   (EV 'IA 6)
   (EV 'KS 6)
   (EV 'KY 8)
   (EV 'LA 8)
   (EV 'ME 4)
   (EV 'MD 10)
   (EV 'MA 11)
   (EV 'MI 16)
   (EV 'MN 10)
   (EV 'MS 6)
   (EV 'MO 10)
   (EV 'MT 3)
   (EV 'NE 5)
   (EV 'NV 6)
   (EV 'NH 4)
   (EV 'NJ 14)
   (EV 'NM 5)
   (EV 'NY 29)
   (EV 'NC 15)
   (EV 'ND 3)
   (EV 'OH 18)
   (EV 'OK 7)
   (EV 'OR 7)
   (EV 'PA 20)
   (EV 'RI 4)
   (EV 'SC 9)
   (EV 'SD 3)
   (EV 'TN 11)
   (EV 'TX 38)
   (EV 'UT 6)
   (EV 'VT 3)
   (EV 'VA 13)
   (EV 'WA 12)
   (EV 'WV 5)
   (EV 'WI 10)
   (EV 'WY 3)))

(: prob-map (Listof DemProb))
;; These probabilities are fabricated. They are loosely modeled on the 
;; Obama/Romney predictions prior to 2012 elections.
(define prob-map
  (list
   (DemProb 'AL 0)
   (DemProb 'AK 0)
   (DemProb 'AZ 0.02)
   (DemProb 'AR 0)
   (DemProb 'CA 1)
   (DemProb 'CO 0.50)
   (DemProb 'CT 1)
   (DemProb 'DE 1)
   (DemProb 'DC 1)
   (DemProb 'FL 0.30)
   (DemProb 'GA 0)
   (DemProb 'HI 1)
   (DemProb 'ID 0)
   (DemProb 'IL 1)
   (DemProb 'IN 0)
   (DemProb 'IA 0.73)
   (DemProb 'KS 0)
   (DemProb 'KY 0)
   (DemProb 'LA 0)
   (DemProb 'ME 0.89)
   (DemProb 'MD 1)
   (DemProb 'MA 1)
   (DemProb 'MI 0.80)
   (DemProb 'MN 0.94)
   (DemProb 'MS 0)
   (DemProb 'MO 0.23)
   (DemProb 'MT 0)
   (DemProb 'NE 0)
   (DemProb 'NV 0.65)
   (DemProb 'NH 0.70)
   (DemProb 'NJ 1)
   (DemProb 'NM 0.87)
   (DemProb 'NY 1)
   (DemProb 'NC 0.20)
   (DemProb 'ND 0)
   (DemProb 'OH 0.50)
   (DemProb 'OK 0)
   (DemProb 'OR 0.90)
   (DemProb 'PA 0.72)
   (DemProb 'RI 1)
   (DemProb 'SC 0)
   (DemProb 'SD 0)
   (DemProb 'TN 0)
   (DemProb 'TX 0.01)
   (DemProb 'UT 0)
   (DemProb 'VT 1)
   (DemProb 'VA 0.50)
   (DemProb 'WA 1)
   (DemProb 'WV 0)
   (DemProb 'WI 0.68)
   (DemProb 'WY 0.02)))
  
(: all-states (Listof State))
(define all-states
  (list 'AL 'AK 'AZ 'AR 'CA 'CO 'CT 'DE 'DC 'FL 'GA 'HI 'ID 'IL 'IN 'IA 'KS
        'KY 'LA 'ME 'MD 'MA 'MI 'MN 'MS 'MO 'MT 'NE 'NV 'NH 'NJ 'NM 'NY 'NC
        'ND 'OH 'OK 'OR 'PA 'RI 'SC 'SD 'TN 'TX 'UT 'VT 'VA 'WA 'WV 'WI 'WY))

;;; === simulation code ===

(: match-state-demprob : State (Listof DemProb) -> Real)
;; [AUX] given a State, return the probability of the democratic party winning
(define (match-state-demprob state prob-map)
  (match prob-map
    [(cons hd tl) (if (equal? state (DemProb-s hd))
                      (DemProb-demp hd)
                      (match-state-demprob state tl))]))
;; [AUX] TESTS
(check-within (match-state-demprob 'AR prob-map) 0 0.001)
(check-within (match-state-demprob 'IL prob-map) 1 0.001)

(: match-state-ev : State (Listof EV) -> Integer)
;; [AUX] given a State, return the number of electoral votes it has
(define (match-state-ev state ev-map)
  (match ev-map
    [(cons hd tl) (if (equal? state (EV-s hd))
                      (EV-ev hd)
                      (match-state-ev state tl))]))
;; [AUX] TESTS
(check-expect (match-state-ev 'AR ev-map) 6)
(check-expect (match-state-ev 'IL ev-map) 20)

(: sim-state : State -> StateResult)
;; given a state, choose a random number on [0,1] and consult 
;; the probability in prob-map above to determine victorious party
;; and look up the number of electoral votes in ev-map
(define (sim-state state)
  (cond
    [(< (random) (match-state-demprob state prob-map))
     ; sim skewed slightly towards 'R since the above uses < instead of <=
     (make-StateResult state 'D (match-state-ev state ev-map))]
    [else (make-StateResult state 'R (match-state-ev state ev-map))]))
;; TESTS
(check-expect (sim-state 'AR) (StateResult 'AR 'R 6))
(check-expect (sim-state 'IL) (StateResult 'IL 'D 20))

; ------------------------------------------------------------------------------

(: myequal-dem : StateResult -> Boolean)
;; [AUX] function for use in filtering StateResults for democratic wins
(define (myequal-dem stateresult)
  (if (equal? 'D (StateResult-p stateresult)) #t #f))
;; [AUX] TESTS
(check-expect (myequal-dem (StateResult 'AL 'R 9)) #f)
(check-expect (myequal-dem (StateResult 'AL 'D 9)) #t)

(: myequal-rep : StateResult -> Boolean)
;; [AUX] function for use in filtering StateResults for democratic wins
(define (myequal-rep stateresult)
  (if (equal? 'R (StateResult-p stateresult)) #t #f))
;; [AUX] TESTS
(check-expect (myequal-rep (StateResult 'AL 'R 9)) #t)
(check-expect (myequal-rep (StateResult 'AL 'D 9)) #f)

(: make-dem-states-list : (Listof StateResult) -> (Listof StateResult))
;; [AUX] filter a list of StateResults and form the democratic victories into a new one
(define (make-dem-states-list list)
  (filter myequal-dem list))
;; [AUX] TESTS
(check-expect (make-dem-states-list (list (StateResult 'AL 'R 9)
                                          (StateResult 'AK 'R 3)
                                          (StateResult 'AZ 'R 11)
                                          (StateResult 'AR 'R 6)
                                          (StateResult 'CA 'D 55)
                                          (StateResult 'CO 'R 9)
                                          (StateResult 'CT 'D 7)))
              (list (StateResult 'CA 'D 55)
                    (StateResult 'CT 'D 7)))

(: make-rep-states-list : (Listof StateResult) -> (Listof StateResult))
;; [AUX] filter a list of StateResults and form the republican victories into a new one
(define (make-rep-states-list list)
  (filter myequal-rep list))
;; [AUX] TESTS
(check-expect (make-rep-states-list (list (StateResult 'AL 'R 9)
                                          (StateResult 'AK 'R 3)
                                          (StateResult 'AZ 'R 11)
                                          (StateResult 'AR 'R 6)
                                          (StateResult 'CA 'D 55)
                                          (StateResult 'CO 'R 9)
                                          (StateResult 'CT 'D 7)))
              (list (StateResult 'AL 'R 9)
                    (StateResult 'AK 'R 3)
                    (StateResult 'AZ 'R 11)
                    (StateResult 'AR 'R 6)
                    (StateResult 'CO 'R 9)))

(: sim-USA : -> USAResult)
;; run simulation on all states (plus Washington, DC)
(define (sim-USA)
  (make-USAResult (make-dem-states-list (map sim-state all-states))
                  (make-rep-states-list (map sim-state all-states))))

; ------------------------------------------------------------------------------

(: ev-extract : StateResult -> Integer)
;; [AUX] function to extract from a StateResult the number of electoral votes
(define (ev-extract stateresult)
  (StateResult-ev stateresult))
;; [AUX] TESTS
(check-expect (ev-extract (StateResult 'AK 'R 3)) 3)
(check-expect (ev-extract (StateResult 'AZ 'R 11)) 11)

(: outcome : USAResult -> Outcome)
;; Add the electoral votes of each candidate to determine outcome.
;; Assume no state splits its electoral votes (in actuality, some do).
(define (outcome usaresult)
  (local
    {(define cum-dem-ev (foldl + 0 (map ev-extract (USAResult-dems usaresult))))
     (define cum-rep-ev (foldl + 0 (map ev-extract (USAResult-reps usaresult))))}
    (cond
      [(> cum-dem-ev
          cum-rep-ev) 'D]
      [(< cum-dem-ev
          cum-rep-ev) 'R]
      [else 'tie])))

; ------------------------------------------------------------------------------

(: list-of-results : Integer -> (Listof Outcome))
;; [AUX] run the simulation a number of times and record the winner of each
;; run-through in a list
(define (list-of-results n)
  (cond
    [(= n 0) '()]
    [else (cons (outcome (sim-USA)) (list-of-results (- n 1)))]))

(: dem-tally : (Listof Outcome) -> Integer)
;; [AUX] given a list of outcomes tallies the number of democratic wins
(define (dem-tally list)
  (match list
    ['() 0]
    [(cons hd tl) (if (equal? hd 'D) (+ 1 (dem-tally tl))
                                     (dem-tally tl))]))
;; [AUX] TESTS
(check-expect (dem-tally '(D R D R D)) 3)
(check-expect (dem-tally '(D D D D tie)) 4)
(check-expect (dem-tally '(R R R R R)) 0)
(check-expect (dem-tally '(tie tie tie R D)) 1)

(: rep-tally : (Listof Outcome) -> Integer)
;; [AUX] given a list of outcomes tallies the number of republican wins
(define (rep-tally list)
  (match list
    ['() 0]
    [(cons hd tl) (if (equal? hd 'R) (+ 1 (rep-tally tl))
                                     (rep-tally tl))]))
;; [AUX] TESTS
(check-expect (rep-tally '(D R D R D)) 2)
(check-expect (rep-tally '(D D D D tie)) 0)
(check-expect (rep-tally '(R R R R R)) 5)
(check-expect (rep-tally '(tie tie tie R D)) 1)

(: tie-tally : (Listof Outcome) -> Integer)
;; [AUX] given a list of outcomes tallies the number of there was a tie
(define (tie-tally list)
  (match list
    ['() 0]
    [(cons hd tl) (if (equal? hd 'tie) (+ 1 (tie-tally tl))
                                       (tie-tally tl))]))
;; [AUX] TESTS
(check-expect (tie-tally '(D R D R D)) 0)
(check-expect (tie-tally '(D D D D tie)) 1)
(check-expect (tie-tally '(R R R R R)) 0)
(check-expect (tie-tally '(tie tie tie R D)) 3)

(: run-sims : Integer -> Tally)
;; given a number of trials to run, run the simulation that
;; number of times, and tally the results over the trials
(define (run-sims n)
  (local
    {(define wins-list (list-of-results n))}
    (make-Tally (dem-tally wins-list)
                (rep-tally wins-list)
                (tie-tally wins-list))))

(test)
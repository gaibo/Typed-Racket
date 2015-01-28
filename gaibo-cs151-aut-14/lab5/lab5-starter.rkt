#lang typed/racket

;; YOUR NAME HERE
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
       
(: sim-state : State -> StateResult)
;; given a state, choose a random number on [0,1] and consult 
;; the probability in prob-map above to determine victorious party
;; and look up the number of electoral votes in ev-map
(define (sim-state s)
  (error "todo"))

(: sim-USA : -> USAResult)
;; run simulation on all states (plus Washington, DC)
(define (sim-USA)
  (error "todo"))
  
(: outcome : USAResult -> Outcome)
;; Add the electoral votes of each candidate to determine outcome.
;; Assume no state splits its electoral votes (in actuality, some do).
(define (outcome r)
  (error "todo"))
  
(: run-sims : Integer -> Tally)
;; given a number of trials to run, run the simulation that
;; number of times, and tally the results over the trials
(define (run-sims n)
  (error "todo"))

#lang typed/racket
(require typed/test-engine/racket-tests)


;; ==== Problem 1 ====

(define-struct Counter
  ([n : Integer])
  #:transparent
  #:mutable)

(: make-increment : Counter -> (Integer -> Void))
;; take a Counter and return a function that itself takes an Integer whose return
;; type is Void, incrementing the Counter by the specified amount when called
(define (make-increment counter)
  (lambda ([increment : Integer])
          (set-Counter-n! counter (- (Counter-n counter)
                                     increment))))
;; TESTS
; Defining a stock counter for testing, incrementing it, then checking it
(define stock-counter1 (make-Counter 10))
((make-increment stock-counter1) 5)
(check-expect (Counter-n stock-counter1) 5)
; Doing it again for good measure
(define stock-counter2 (make-Counter 10))
((make-increment stock-counter2) 6)
(check-expect (Counter-n stock-counter2) 4)


;; ==== Problem 2 ====

(define-type (VTree A) (Vectorof A))

(: in-tree? : (All (A) (VTree A) Integer -> Boolean))
;; tell whether an index is in the tree
(define (in-tree? vtree n)
  (< n (vector-length vtree)))  ; less-than is sufficient to account for 0 index
;; TESTS
(check-expect (in-tree? (vector 1 2) 2) #f)
(check-expect (in-tree? (vector 1 2) 3) #f)
(check-expect (in-tree? (vector 1 2 3) 2) #t)
(check-expect (in-tree? (vector 1 2 3) 0) #t)

(: left-kid  : Integer -> Integer)
;; compute the index of the left child
(define (left-kid n)
  (+ (* 2 n) 1))
;; TESTS
(check-expect (left-kid 0) 1)
(check-expect (left-kid 1) 3)

(: right-kid  : Integer -> Integer)
;; compute the index of the right child
(define (right-kid n)
  (+ (* 2 n) 2))
;; TESTS
(check-expect (right-kid 0) 2)
(check-expect (right-kid 1) 4)

(: parent : Integer -> Integer)
;; compute the index of the parent
(define (parent n)
  (if (odd? n) (quotient (- n 1) 2)  ; convenient way to evaluate cases
               (quotient (- n 2) 2)))
;; TESTS
(check-expect (parent 3) (parent 4))
(check-expect (parent 1) (parent 2))

(: vtree->list : (All (A) (VTree A) -> (Listof A)))
;; return the values of a VTree according to a left-to-right traversal
(define (vtree->list vtree)
  (local
    {(: list-maker : Integer -> (Listof A))
     ;; recursively put together the list
     (define (list-maker index)
       (cond
         [(not (in-tree? vtree (left-kid index)))
          (list (vector-ref vtree index))]
         [else (append (list-maker (left-kid index))
                       (list (vector-ref vtree index))
                       (list-maker (right-kid index)))]))}
    (list-maker 0)))
;; TESTS
(check-expect (vtree->list (vector "a" "b" "c"))
              (list "b" "a" "c"))
(check-expect (vtree->list (vector "a" "b" "c" "d" "e" "f" "g"))
              (list "d" "b" "e" "a" "f" "c" "g"))
(check-expect (vtree->list (vector "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k"
                                   "l" "m" "n" "o"))
              (list "h" "d" "i" "b" "j" "e" "k" "a" "l" "f" "m" "c" "n" "g" "o"))
(check-expect (vtree->list (vector 1 4 9 16 25))
              (list 16 4 25 1 9))

(: bst-vtree-member? : (VTree Integer) Integer -> Boolean)
;; determine whether a key is in the binary search VTree
(define (bst-vtree-member? vtree key)
  (local
    {(: search : Integer -> Boolean)
     ;; search an index in the tree recursively for the key
     (define (search index)
       (if (in-tree? vtree index)
           (cond
             [(= key (vector-ref vtree index)) #t]
             [(> key (vector-ref vtree index)) (search (right-kid index))]
             [else (search (left-kid index))])
           #f))}
    (search 0)))
;; TESTS
(check-expect (bst-vtree-member? (vector 16 4 25 1 9) 1) #t)
(check-expect (bst-vtree-member? (vector 16 4 25 1 9) 4) #t)
(check-expect (bst-vtree-member? (vector 16 4 25 1 9) 25) #t)
(check-expect (bst-vtree-member? (vector 16 4 25 1 9) 40) #f)


;; ==== Problem 3 ====

(: rotate-right : (All (A) (Vectorof A) Integer Integer -> Void))
;; rotate a contiguous sub-sequence of vector elements to the right
;; by one position
(define (rotate-right vector lo hi)
  (local
    {(define hi-val (vector-ref vector hi))
     (: move-up-to-cover : Integer -> Void)
     ;; move the values up recursively to cover the values to their right
     (define (move-up-to-cover pos)
       (if (> pos lo)
           (begin (vector-set! vector pos (vector-ref vector (sub1 pos)))
                  (move-up-to-cover (sub1 pos)))
           (void)))}
    (begin (move-up-to-cover hi)
           (vector-set! vector lo hi-val))))
;; TESTS
(check-expect (begin (define v (vector 0 1 2 3 4 5 6 7 8 9))
                     (rotate-right v 2 5)
                     v)
              (vector 0 1 5 2 3 4 6 7 8 9))
(check-expect (begin (define v (vector 0 1))
                     (rotate-right v 0 1)
                     v)
              (vector 1 0))


;; ==== Problem 4 ====

(define-struct (A) Vertex 
  ([id : Integer]
   [attr : A]
   [succs : (Listof Integer)])
  #:transparent
  #:mutable)

(define-type (Graph A) (Vectorof (Vertex A)))

; ------------------------------------------------------------------------------
; Some stock graphs for later testing
(define stock-graph (vector (make-Vertex 1 "a" (list 2 3))
                            (make-Vertex 2 "b" '())
                            (make-Vertex 3 "c" '())))

(define stock-graph-large (vector (make-Vertex 1 "a" (list 2 3))
                                  (make-Vertex 2 "b" (list 4 5))
                                  (make-Vertex 3 "c" '())
                                  (make-Vertex 4 "d" '())
                                  (make-Vertex 5 "e" '())))
; ------------------------------------------------------------------------------

(: find-vertex : (All (A) (Graph A) Integer -> (Vertex A)))
;; [AUX] find a vertex corresponding to an id
(define (find-vertex graph id)
  (match (filter (lambda ([vertex : (Vertex A)])
                         (= id (Vertex-id vertex)))
                 (vector->list graph))
    [(cons hd '()) hd]
    ['() (error "specified vertex not found")]))
;; [AUX] TESTS
(check-expect (find-vertex stock-graph 1)
              (make-Vertex 1 "a" (list 2 3)))
(check-error (find-vertex stock-graph 4)
             "specified vertex not found")

(: valid-edge? : (All (A) (Graph A) -> (Integer Integer -> Boolean)))
;; given a Graph, return a curried function for testing whether an edge between
;; two vertices exists
(define (valid-edge? graph)
  (local
    {(: look-through-succs : (Listof Integer) Integer -> Boolean)
     ;; look through a list of id's to see if the specified one is there
     (define (look-through-succs list id-2)
       (match list
         ['() #f]
         [(cons hd tl) (if (= hd id-2) #t (look-through-succs tl id-2))]))}
    (lambda ([id-1 : Integer] [id-2 : Integer])
            (look-through-succs (Vertex-succs (find-vertex graph id-1))
                                id-2))))
;; TESTS
(check-expect ((valid-edge? stock-graph) 1 2) #t)
(check-expect ((valid-edge? stock-graph) 1 3) #t)
(check-expect ((valid-edge? stock-graph) 2 1) #f)
(check-expect ((valid-edge? stock-graph) 2 3) #f)

(: valid-path? : (All (A) (Graph A) (Listof Integer) -> Boolean))
;; given a Graph and a list of Vertex indices, determine whether the vertices are
;; all connected by edges in order
(define (valid-path? graph ids-list)
  (local
    {(: bool-list : Integer (Listof Integer) -> (Listof Boolean))
     ;; generate a list of Booleans indicating whether there exists an edge
     ;; between each two vertices
     (define (bool-list last-id remaining-ids-list)
       (match remaining-ids-list
         ['() '()]
         [(cons hd tl) (cons ((valid-edge? graph) last-id hd)
                             (bool-list hd tl))]))}
    (cond
      [(or (= (length ids-list) 0)
           (= (length ids-list) 1)) #t]
      ; you can always get to yourself(bool-list ids-list
      [else (andmap (lambda ([bool : Boolean])
                            (equal? #t bool))
                    (bool-list (first ids-list) (rest ids-list)))])))
;; TESTS
(check-expect (valid-path? stock-graph-large (list 1 2 4)) #t)
(check-expect (valid-path? stock-graph-large (list 1 2 5)) #t)
(check-expect (valid-path? stock-graph-large (list 1 2 3)) #f)
(check-expect (valid-path? stock-graph-large (list 1 2)) #t)

(test)

;; === automated test code follows

;; == some type abbreviations for readability

(define-type Int Integer)
(define-type Bool Boolean)
(define-type (Vec α) (Vectorof α))

(: check-points-handler : String -> (Any -> Void))
;; custom exception handler for check-points, given problem number
;; - displays message about the exception raised
(define ((check-points-handler problem-name) x)
   (display (string-append "\ncheck-points: "
                           "error raised while testing problem "
                           problem-name
                           (if (exn? x)
                               (local {(define msg (exn-message x))}
                                 (if (string=? msg "")
                                     ": <empty error message>"
                                     (string-append ":\n" msg)))
                               "")
                           "\n")))
  
(: check-points : (Vec String) (Vec Int) -> (Int Int (-> Bool) -> Void))
;; run the test, record points earned in points vector
(define ((check-points problem-names scores) prob pts run-test)
  (local {(define prob-name (vector-ref problem-names prob))}
    (with-handlers 
        ([exn? (check-points-handler prob-name)])
      (match (run-test)
        [#t (vector-set! scores prob (+ pts (vector-ref scores prob)))]
        [result 
         (begin
           (display (string-append "\ncheck-points: "
                                   "unexpected result testing problem "
                                   prob-name
                                   "\n"))
           (display "unexpected result: ")
           (display result)
           (newline))]))))


(: problem : (Vec String) (Vec Int) -> (Int String Int -> Void))
;; register problem in problem name and max scores vectors
(define ((problem problem-names max-scores) i s pts)
  (begin
    (vector-set! problem-names i s)
    (vector-set! max-scores i pts)))

(: grade-report : (Vec String) (Vec Int) (Vec Int) -> Void)
;; display information about results of all tests
(define (grade-report problem-names scores max-scores)
  (local 
    {(define as-list (vector->list scores))
     (define total (foldl + 0 as-list))
     (: print-result : Integer -> Void)
     (define (print-result i)
       (display (string-append
                 ";; problem " (vector-ref problem-names i) " : "
                 (number->string (vector-ref scores i))
                 "/" (number->string (vector-ref max-scores i)) "\n")))}
    (begin
      (newline)
      (display ";; === evaluation\n\n")
      (build-list 10 print-result)
      (display (string-append "\n;; _total-" "score_ " ;; so grep isn't fooled
                              (number->string total) "/50\n")))))

(display (string-append "\n\n;; =========================================\n"
                        ";; === begin automated evaluation of hw7 ===\n"
                        ";; =========================================\n"))

(define problem-names-vec (make-vector 10 ""))
(define grading-scores-vec (make-vector 10 0))
(define max-scores-vec (make-vector 10 0))

(define prob (problem problem-names-vec max-scores-vec))
(define check (check-points problem-names-vec grading-scores-vec))

;;;;;;;;;;; PROBLEM 1 ;;;;;;;;;;

(prob 0 "1 (mutable counter)" 6)
(check 0 6
 (lambda ()
 (local
   {(define cntr (Counter 0))
    (define inc-cntr (make-increment cntr))}
   (begin
     (inc-cntr 5)
     (inc-cntr -3)
     (inc-cntr 4)
     (match cntr
       [(Counter 6) #t]
       [else #f])))))

;;;;;;;;;;; PROBLEM 2 ;;;;;;;;;;

(define gr_test-tree : (VTree Integer) (vector 12 9 16 5 10 14))

;;;;; Part a

(prob 1 "2a (in-tree?)" 2)
(check 1 1
  (lambda ()
    (in-tree? gr_test-tree 3))) ;; also 0
(check 1 1
  (lambda ()
   (not (in-tree? gr_test-tree -1))))

;;;;; Part b

(prob 2 "2b (left-kid)" 2)
(prob 3 "2b (right-kid)" 2)
(prob 4 "2b (parent)" 3)
(check 2 1
  (lambda ()
    (= (left-kid 3) 7)))
(check 3 1
  (lambda ()
    (= (right-kid 3) 8)))
(check 2 1
  (lambda ()
    (= (left-kid 4) 9)))
(check 3 1
  (lambda ()
    (= (right-kid 4) 10)))
(check 4 1
  (lambda ()
    (= (parent (left-kid 3)) 3)))
(check 4 1
  (lambda ()
    (= (parent (right-kid 3)) 3)))
(check 4 1
  (lambda ()
    (= (parent (left-kid 4)) 4))) ;; also right

;;;;; Part c

(prob 5 "2c (vtree->list)" 8)
(check 5 8
  (lambda ()
    (equal? (vtree->list gr_test-tree) (list 5 9 10 12 14 16))))

;;;;; Part d

(prob 6 "2d (bst-vtree-member?)" 8)
(check 6 1
  (lambda ()
    (not (bst-vtree-member? gr_test-tree 0))))
(check 6 1
  (lambda ()
    (bst-vtree-member? gr_test-tree 5)))
(check 6 1
  (lambda ()
    (bst-vtree-member? gr_test-tree 12)))
(check 6 1
  (lambda ()
    (not (bst-vtree-member? gr_test-tree 13))))
(check 6 2
  (lambda ()
    (bst-vtree-member? gr_test-tree 16)))
(check 6 2
  (lambda ()
    (not (bst-vtree-member? gr_test-tree 18))))

;;;;;;;;;; PROBLEM 3 ;;;;;;;;;;

(prob 7 "3 (rotate-right)" 10)
(check 7 5
  (lambda ()
    (local {(define v (vector 0 1 2 3 4 5 6 7 8 9))}
      (begin (rotate-right v 0 9)
        (equal? v (vector 9 0 1 2 3 4 5 6 7 8))))))
(check 7 5
  (lambda ()
    (local {(define v (vector 0 1 2 3 4 5 6 7 8 9))}
      (begin (rotate-right v 2 3)
        (equal? v (vector 0 1 3 2 4 5 6 7 8 9))))))

;;;;;;;;;; PROBLEM 4 ;;;;;;;;;;

(define gr_test-g : (Graph Symbol)
  (vector
   (Vertex 0 'A '(1 4))  ;; A -> B; E
   (Vertex 1 'B '(4 5))  ;; B -> E; F
   (Vertex 2 'C '(3))    ;; C -> D
   (Vertex 3 'D '())
   (Vertex 4 'E '(2 5))  ;; E -> C; F
   (Vertex 5 'F '(3 6))  ;; F -> D; G
   (Vertex 6 'G '())))

(prob 8 "4 (valid-edge?)" 3)
(prob 9 "4 (valid-path?)" 6)

;;;;; Part a

(check 8 1
  (lambda ()
    (not ((valid-edge? gr_test-g) 1 0))))
(check 8 1
  (lambda ()
    ((valid-edge? gr_test-g) 2 3)))
(check 8 1
  (lambda ()
    (not ((valid-edge? gr_test-g) 3 4))))
;; also 0-1, 0-2 (not), 0-4

;;;;; Part b

(check 9 1
  (lambda ()
    (valid-path? gr_test-g '())))
(check 9 1
  (lambda ()
    (valid-path? gr_test-g '(1)) #t))
(check 9 1
  (lambda ()
    (valid-path? gr_test-g '(0 4 2 3)) #t))
(check 9 3
  (lambda ()
    (not (valid-path? gr_test-g '(0 4 2 3 6)))))

(grade-report problem-names-vec grading-scores-vec max-scores-vec)

All 32 tests passed!


;; =========================================
;; === begin automated evaluation of hw7 ===
;; =========================================

check-points: unexpected result testing problem 1 (mutable counter)
unexpected result: #f

check-points: unexpected result testing problem 2a (in-tree?)
unexpected result: #f

check-points: error raised while testing problem 2c (vtree->list):
vector-ref: index is out of range
  index: 6
  valid range: [0, 5]
  vector: '#(12 9 16 5 10 14)

;; === evaluation

;; problem 1 (mutable counter) : 0/6
;; problem 2a (in-tree?) : 1/2
;; problem 2b (left-kid) : 2/2
;; problem 2b (right-kid) : 2/2
;; problem 2b (parent) : 3/3
;; problem 2c (vtree->list) : 0/8
;; problem 2d (bst-vtree-member?) : 8/8
;; problem 3 (rotate-right) : 10/10
;; problem 4 (valid-edge?) : 3/3
;; problem 4 (valid-path?) : 6/6

;; _total-score_ 35/50

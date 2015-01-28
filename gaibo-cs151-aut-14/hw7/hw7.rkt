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
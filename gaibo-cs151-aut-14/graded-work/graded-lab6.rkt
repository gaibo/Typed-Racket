#lang typed/racket
(require typed/test-engine/racket-tests)

;; Gaibo Zhang
;; Wachs 151
;; Lab 6


;; === polymorphic data structures (broadly useful)

(define-struct (a b) Pair
  ([first  : a]
   [second : b])
  #:transparent)

(define-type (Maybe a) (U 'Nothing (Just a)))

(define-struct (a) Just
  ([x : a])
  #:transparent)

(define-type Order (U 'Less 'Equal 'Greater))

(define-type (Cmp a) (a a -> Order))


;; === BST map data structures

(define-struct (key val) BSTMap
  ([compare : (Cmp key)]
   [data    : (BST key val)])
  #:transparent)

(define-type (BST key val) (U 'E (Node key val)))
;; note the symbol 'E is used for the empty tree

(define-struct (key val) Node
  ([root : key]
   [v    : val]
   [lsub : (BST key val)]
   [rsub : (BST key val)])
  #:transparent)


;; === BST map operations

; ------------------------------------------------------------------------------
;; Some stock cmps and trees and maps for later testing
(: cmp-int : (Cmp Integer))
(define (cmp-int m n)
  (cond
    [(< m n) 'Less]
    [(= m n) 'Equal]
    [else 'Greater]))

(: fig1-tree : (BST Integer String))
(define fig1-tree
  (make-Node 5
             "Regenstein"
             (make-Node 1
                        "Ryerson"
                        (make-Node 0
                                   "Harper"
                                   'E
                                   'E)
                        (make-Node 3
                                   "Crerar"
                                   'E
                                   'E))
             (make-Node 8
                        "Pick"
                        (make-Node 6
                                   "Kent"
                                   'E
                                   'E)
                        (make-Node 9
                                   "Ratner"
                                   'E
                                   'E))))

(: fig1-map : (BSTMap Integer String))
(define fig1-map
  (make-BSTMap cmp-int fig1-tree))

(: fig1-noratner-tree : (BST Integer String))
(define fig1-noratner-tree
  (make-Node 5
             "Regenstein"
             (make-Node 1
                        "Ryerson"
                        (make-Node 0
                                   "Harper"
                                   'E
                                   'E)
                        (make-Node 3
                                   "Crerar"
                                   'E
                                   'E))
             (make-Node 8
                        "Pick"
                        (make-Node 6
                                   "Kent"
                                   'E
                                   'E)
                        'E)))

(: fig1-noratner-map : (BSTMap Integer String))
(define fig1-noratner-map
  (make-BSTMap cmp-int fig1-noratner-tree))

(: fig1-removed-root-tree : (BST Integer String))
(define fig1-removed-root-tree
  (make-Node 3
             "Crerar"
             (make-Node 1
                        "Ryerson"
                        (make-Node 0
                                   "Harper"
                                   'E
                                   'E)
                        'E)
             (make-Node 8
                        "Pick"
                        (make-Node 6
                                   "Kent"
                                   'E
                                   'E)
                        (make-Node 9
                                   "Ratner"
                                   'E
                                   'E))))

(: fig1-removed-root-map : (BSTMap Integer String))
(define fig1-removed-root-map
  (make-BSTMap cmp-int fig1-removed-root-tree))
; ------------------------------------------------------------------------------

(: map-apply : (All (key val) (BSTMap key val) key -> (Maybe val)))
;; if the key is present in the tree, return the associated value in a Just;
;; otherwise, return 'Nothing
(define (map-apply m k)
  (match m
    [(BSTMap cmp t)
     (local
       {(: lp : (BST key val) -> (Maybe val))
        ;; search for k in t
        (define (lp t)
          (match t
            ['E 'Nothing]
            [(Node r v tl tr)
             (match (cmp k r)
               ['Less (lp tl)]
               ['Equal (Just v)]
               ['Greater (lp tr)])]))}
       (lp t))]))
;; TESTS
(check-expect (map-apply fig1-map 5) (Just "Regenstein"))
(check-expect (map-apply fig1-map 1) (Just "Ryerson"))
(check-expect (map-apply fig1-map 6) (Just "Kent"))
(check-expect (map-apply fig1-map 17) 'Nothing)

(: insert : (All (key val) (BSTMap key val) key val -> (BSTMap key val)))
;; insert key/value pair at the correct position in the tree; if key is already
;; present, replace it
(define (insert m k v)
  (match m
    [(BSTMap cmp t)
     (local
       {(: tree-insert : (BST key val) -> (BST key val))
        ;; insert a node into the tree
        (define (tree-insert tree)
          (match tree
            ['E (make-Node k v 'E 'E)]
            [(Node root val lsub rsub) 
             (match (cmp k root)
               ['Less (make-Node root
                                 val
                                 (tree-insert lsub)
                                 rsub)]
               ['Equal (make-Node k v lsub rsub)]
               ['Greater (make-Node root
                                    val
                                    lsub
                                    (tree-insert rsub))])]))}
       (make-BSTMap cmp (tree-insert t)))]))
;; TESTS
(check-expect (BSTMap-data (insert fig1-map 3 "Crerar")) fig1-tree)
(check-expect (BSTMap-data (insert fig1-map 5 "Regenstein")) fig1-tree)
(check-expect (BSTMap-data (insert fig1-noratner-map 9 "Ratner"))
              fig1-tree)

(: remove-max : (All (key val) (BST key val) -> (Pair (Pair key val)
                                                      (BST key val))))
;; remove the max keyholder node from the tree and give the removed key/value
;; as well as the new tree, assuming the tree and both subtrees are nonempty
(define (remove-max tree)
  (local
    {(: removed-max : (BST key val) -> (Pair key val))
     ;; return the paired key and val of the removed keyholder
     (define (removed-max tree)
       (match tree
         [(Node root val _ rsub)
          (match rsub
            ['E (make-Pair root val)]
            [(Node _ _ _ _) (removed-max rsub)])]))
     (: removed-max-tree : (BST key val) -> (BST key val))
     ;; return the tree with removed max keyholder
     (define (removed-max-tree tree)
       (match tree
         [(Node root val lsub rsub)
           (match rsub
             ['E 'E]
             [(Node _ _ _ _) (make-Node root
                                        val
                                        lsub
                                        (removed-max-tree rsub))])]))}
    (make-Pair (removed-max tree) (removed-max-tree tree))))

;; #Grader - can drop the rest of a three in depth = 2 case
;; TESTS
(check-expect (remove-max fig1-tree) (Pair (Pair 9 "Ratner") fig1-noratner-tree))

(: remove-root : (All (key val) (BST key val) -> (BST key val)))
;; remove the root keyholder node and switch the max keyholder of the node
;; into the root position
(define (remove-root tree)
  (match tree
    ['E tree]
    [(Node _ _ 'E rsub) rsub]
    [(Node _ _ lsub 'E) lsub]
    [(Node _ _ lsub rsub)
     (local
       {(define remove-max-lsub (remove-max lsub))} ; save some later runs
       (make-Node (Pair-first
                    (Pair-first remove-max-lsub))
                  (Pair-second
                    (Pair-first remove-max-lsub))
                  (Pair-second remove-max-lsub)
                  rsub))]))
;; TESTS
(check-expect (remove-root (Node 1 "test" 'E 'E)) 'E)
(check-expect (remove-root fig1-tree) fig1-removed-root-tree)

(: remove : (All (key val) (BSTMap key val) key -> (BSTMap key val)))
;; remove the keyholder node from the tree
(define (remove m k)
  (match m
    [(BSTMap cmp tree)
     (local
       {(: remove-tree : (BST key val) -> (BST key val))
        ;; remove the node now that the map is unpacked
        (define (remove-tree tree)
          (match tree
            ['E tree]
            [(Node root val lsub rsub)
             (match (cmp k root)
               ['Less (make-Node root
                                 val
                                 (remove-tree lsub)
                                 rsub)]
               ['Equal (remove-root tree)]
               ['Greater (make-Node root
                                    val
                                    lsub
                                    (remove-tree rsub))])]))}
       (make-BSTMap cmp (remove-tree tree)))]))
;; TESTS
(check-expect (BSTMap-data (remove fig1-map 9)) fig1-noratner-tree)
(check-expect (BSTMap-data (remove fig1-map 5)) fig1-removed-root-tree)

;; === graders' tests follow

(: grader-test:singleton : (All (key val) key val -> (BST key val)))
(define (grader-test:singleton k v)
  (Node k v 'E 'E))

(check-expect (grader-test:singleton 1 "A") (Node 1 "A" 'E 'E))

(: grader-test:empty-IS : (BSTMap Integer String))
;; empty Int -> String map
(define grader-test:empty-IS 
  (BSTMap cmp-int 'E))

(: grader-test:fig1-tree (BST Integer String))
(define grader-test:fig1-tree
  (Node 5 "Regenstein"
        (Node 1 "Ryerson"
              (Node 0 "Harper" 'E 'E)
              (Node 3 "Crerar" 'E 'E))
        (Node 8 "Pick"
              (Node 6 "Kent" 'E 'E)
              (Node 9 "Ratner" 'E 'E))))

(: grader-test:fig1-map (BSTMap Integer String))
(define grader-test:fig1-map
  (BSTMap cmp-int grader-test:fig1-tree))

(check-expect (map-apply grader-test:fig1-map 6)  (Just "Kent"))
(check-expect (map-apply grader-test:fig1-map 1)  (Just "Ryerson"))
(check-expect (map-apply grader-test:fig1-map 2)  'Nothing)
(check-expect (map-apply grader-test:fig1-map 99) 'Nothing)

(check-expect
 (BSTMap-data (insert grader-test:empty-IS 5 "Regenstein"))
 (grader-test:singleton 5 "Regenstein"))

(check-expect
 (BSTMap-data (insert (insert grader-test:empty-IS 5 "Regenstein") 1 "Ryerson"))
 (Node 5 "Regenstein" (grader-test:singleton 1 "Ryerson") 'E))

(check-expect (remove-max (grader-test:singleton 9 "Ratner"))
              (Pair (Pair 9 "Ratner") 'E))

(check-expect (remove-max (Node 9 "Ratner" (grader-test:singleton 3 "Crerar") 'E))
              (Pair (Pair 9 "Ratner") (grader-test:singleton 3 "Crerar")))

(check-expect (remove-max grader-test:fig1-tree)
              (Pair (Pair 9 "Ratner")
                    (Node 5 "Regenstein"
                          (Node 1 "Ryerson"
                                (Node 0 "Harper" 'E 'E)
                                (Node 3 "Crerar" 'E 'E))
                          (Node 8 "Pick"
                                (Node 6 "Kent" 'E 'E)
                                'E))))

(check-expect
 (remove-root (Node 5 "Regenstein" (Node 1 "Ryerson" 'E 'E) 'E))
 (Node 1 "Ryerson" 'E 'E))

(check-expect
 (remove-root (Node 5 "Regenstein" 
                    (Node 1 "Ryerson" 'E (Node 3 "Crerar" 'E 'E))
                       (Node 6 "Kent" 'E 'E)))
 (Node 3 "Crerar" (grader-test:singleton 1 "Ryerson") (grader-test:singleton 6 "Kent")))



(define mmm (BSTMap cmp-int (Node 5 "Regenstein" 
                                   (Node 1 "Ryerson" 'E (grader-test:singleton 3 "Crerar"))
                                   (grader-test:singleton 6 "Kent"))))

(check-expect
 (BSTMap-data (remove mmm 5))
 (Node 3 "Crerar" (grader-test:singleton 1 "Ryerson") (grader-test:singleton 6 "Kent")))

(check-expect
 (BSTMap-data (remove mmm 1))
 (Node 5 "Regenstein" (grader-test:singleton 3 "Crerar") (grader-test:singleton 6 "Kent")))

(check-expect (BSTMap-data (remove mmm 99)) (BSTMap-data mmm))

;; evaluation

;; === correctness ===

;; map-apply (given)                 2/ 2

;; insert                           10/10

;; remove-max                        9/10
;; remove-root                      10/10
;; remove                           10/10

;; === style ===

;; code layout                       8/ 8
;; identifiers are well named        8/ 8  #Grader - changing names is fine as you made a note
;; program organization              4/ 4

;; contracts (types)                 8/ 8
;; well-written purposes             8/ 8
;; adequate tests                   10/10

;; clarity (clear logic)             6/ 6

;; svn used correctly                6/ 6

;; _total-score_                    99/ 100

;; graded by Calvin Deutschbein

(test)
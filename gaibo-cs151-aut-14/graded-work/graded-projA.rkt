#lang typed/racket
(require typed/test-engine/racket-tests)
(require/typed 2htdp/image
   [#:opaque Image image?]
   [rectangle (-> Number Number String String Image)]
   [image-width (-> Image Number)]
   [image-height (-> Image Number)]
   [text (-> String Number String Image)] 
   [beside (-> Image * Image)]
   [beside/align (-> String Image * Image)]
   [above (-> Image * Image)]
   [above/align (-> String Image * Image)]
   [overlay (-> Image * Image)]
   [overlay/xy (-> Image Number Number Image Image)]
   [crop (-> Number Number Number Number Image Image)]
   [flip-vertical (-> Image Image)]
   [flip-horizontal (-> Image Image)]
   [freeze (-> Image Image)]
   [rotate (-> Number Image Image)]
   [empty-image Image]
   [circle (-> Number String String Image)]
   [square (-> Number String String Image)]
   [triangle (-> Number String String Image)]
   [star (-> Number String String Image)]
   [radial-star (-> Number Number Number String String Image)])

;; Gaibo Zhang
;; Project Part A
;; CS151 Wachs

;; ==== Data Definitions ====

(define-type Player (U 'black 'white))

(define-struct Pos
  ([row : Integer]  ;; an integer on the interval [0,7]
   [col : Integer]) ;; an integer on the interval [0,7]
  #:transparent)

(define-struct Board
  ([squares : (Listof (U Player 'none))]) ;; a list of length 64
  #:transparent)

(define-struct Game
  ([board : Board]
   [next  : Player])
  #:transparent)

;; ==== Functional Code Start ====

(: new-board : Integer -> Board)
;; [AUX] [SCALABLE] create a new Game Board with the given number of squares
(define (new-board n) ; n must be an even perfect square
  (make-Board
   (local
     {(define orig-n n)
      (: square-list-maker : Integer -> (Listof (U Player 'none)))
      ;; create the list of squares
      (define (square-list-maker n)
        (cond
          [(= n 0) empty]
          [(= n (- (/ orig-n 2) (/ (sqrt orig-n) 2)))
           ; algorithm for determining starting four points
           ; used three more times
           (cons 'white (square-list-maker (- n 1)))]
          [(= n (- (/ orig-n 2) (- (/ (sqrt orig-n) 2) 1)))
           (cons 'black (square-list-maker (- n 1)))]
          [(= n (+ (/ orig-n 2) (/ (sqrt orig-n) 2)))
           (cons 'black (square-list-maker (- n 1)))]
          [(= n (+ (/ orig-n 2) (+ (/ (sqrt orig-n) 2) 1)))
           (cons 'white (square-list-maker (- n 1)))]
          [else (cons 'none (square-list-maker (- n 1)))]))}
     (square-list-maker n))))

(: new-game : Game)
;; create a Game structure representing the beginning of a new game
(define new-game
  (make-Game (new-board 64)
             'black))

(: board-ref : Board Pos -> (U Player 'none))
;; return the contents of the specified square
(define (board-ref board pos)
  (list-ref (Board-squares board)
            (+ (* 8 (Pos-row pos)) (Pos-col pos)))) ; algorithm for mapping

(: opponent : Player -> Player)
;; [AUX] return the opponent's color
(define (opponent player)
  (if (equal? player 'black) 'white 'black))

(: pos+ : Pos Pos -> Pos)
;; [AUX] add two positions
(define (pos+ pos1 pos2)
  (make-Pos (+ (Pos-row pos1) (Pos-row pos2))
            (+ (Pos-col pos1) (Pos-col pos2))))

(: pos- : Pos Pos -> Pos)
;; [AUX] subtract two positions to get a direction vector
(define (pos- pos1 pos2)
  (make-Pos (- (Pos-row pos1) (Pos-row pos2))
            (- (Pos-col pos1) (Pos-col pos2))))

(: on-board? : Pos -> Boolean)
;; [AUX] tell whether a position is on the 8x8 board
(define (on-board? pos)
  (and (>= (Pos-row pos) 0)
       (<= (Pos-row pos) 7)
       (>= (Pos-col pos) 0)
       (<= (Pos-col pos) 7)))

(: outflanks? : Board Player Pos -> Boolean)
;; return true if the player can place a piece on a position on a board and be
;; surrounding an opponent piece either vertically, horizontally, or diagonally
(define (outflanks? board player pos)
  (local {
    (define opp-color (opponent player))
    (: neighboring-possible : (Listof Pos))
    ;; generate the immediately surrounding eight points, or however many there
    ;; are without going off the board
    (define neighboring-possible
      (filter on-board? (list (pos+ pos (make-Pos -1 -1))
                              (pos+ pos (make-Pos -1 0))
                              (pos+ pos (make-Pos -1 1))
                              (pos+ pos (make-Pos 0 1))
                              (pos+ pos (make-Pos 1 1))
                              (pos+ pos (make-Pos 1 0))
                              (pos+ pos (make-Pos 1 -1))
                              (pos+ pos (make-Pos 0 -1)))))
    (: opponent-directions : (Listof Pos) -> (Listof Pos))
    ;; determine which of the neighboring positions contain opponent pieces and
    ;; create a list containing them
    (define (opponent-directions poslist)
      (match poslist
        ['() '()]
        [(cons hd tl)
         (cond
           [(equal? (board-ref board hd) opp-color)
            (cons hd (opponent-directions tl))]
           [else (opponent-directions tl)])]))
    (: directions-to-search : (Listof Pos) -> (Listof Pos))
    ;; give a list of direction vectors to search for end pieces
    (define (directions-to-search poslist)
      (match poslist
        ['() '()]
        [(cons hd tl) (cons (pos- hd pos) (directions-to-search tl))]))
    (: go-down-direction : Pos Pos -> Boolean)
    ;; tell whether a direction is flanked
    (define (go-down-direction pos dir)
      (cond
        [(not (on-board? (pos+ pos dir))) #f]
        [(equal? (board-ref board (pos+ pos dir)) opp-color)
         (go-down-direction (pos+ pos dir) dir)]
        [(and (equal? (board-ref board (pos+ pos dir)) player)
              (equal? (board-ref board pos) opp-color)) #t]
        [else #f]))
    (: go-down-direction-using-pos : Pos -> Boolean)
    ;; a single-input version of go-down-direction
    (define (go-down-direction-using-pos dir)
      (go-down-direction pos dir))}
  (ormap go-down-direction-using-pos
         (directions-to-search
           (opponent-directions
             neighboring-possible)))))

(: flips : Board Player Pos -> (Listof Pos))
;; return the possibly empty list of positions containing pieces to flip if a
;; move to outflank is enacted
(define (flips board player pos)
  (local {
    (define opp-color (opponent player))
    (: neighboring-possible : (Listof Pos))
    ;; generate the immediately surrounding eight points, or however many there
    ;; are without going off the board
    (define neighboring-possible
      (filter on-board? (list (pos+ pos (make-Pos -1 -1))
                              (pos+ pos (make-Pos -1 0))
                              (pos+ pos (make-Pos -1 1))
                              (pos+ pos (make-Pos 0 1))
                              (pos+ pos (make-Pos 1 1))
                              (pos+ pos (make-Pos 1 0))
                              (pos+ pos (make-Pos 1 -1))
                              (pos+ pos (make-Pos 0 -1)))))
    (: opponent-directions : (Listof Pos) -> (Listof Pos))
    ;; determine which of the neighboring positions contain opponent pieces and
    ;; create a list containing them
    (define (opponent-directions poslist)
      (match poslist
        ['() '()]
        [(cons hd tl)
         (cond
           [(equal? (board-ref board hd) opp-color)
            (cons hd (opponent-directions tl))]
           [else (opponent-directions tl)])]))
    (: directions-to-search : (Listof Pos) -> (Listof Pos))
    ;; give a list of direction vectors to search for end pieces
    (define (directions-to-search poslist)
      (match poslist
        ['() '()]
        [(cons hd tl) (cons (pos- hd pos) (directions-to-search tl))]))
    (: go-down-direction : Pos Pos -> (Listof Pos))
    ;; recursively go down a direction and output a list of all the points that
    ;; contain an opp-color between two player colors
    (define (go-down-direction pos dir)
      (if (equal? (outflanks? board player pos) #t)
          (cond
            [(not (on-board? (pos+ pos dir))) '()]
            [(equal? (board-ref board (pos+ pos dir)) opp-color)
             (cons (pos+ pos dir) (go-down-direction (pos+ pos dir) dir))]
            [(and (equal? (board-ref board (pos+ pos dir)) player)
                  (equal? (board-ref board pos) opp-color))
              '()]
            [else '()])
          '()))
    (: go-down-direction-using-pos : Pos -> (Listof Pos))
    ;; a single-input version of go-down-direction
    (define (go-down-direction-using-pos dir)
      (go-down-direction pos dir))
    (: flatten : (Listof (Listof Pos)) -> (Listof Pos))
    ;; convert a list of lists into just a list
    (define (flatten list)
      (match list
        ['() '()]
        [(cons hd tl) (append hd (flatten tl))]))}
  (flatten (map go-down-direction-using-pos (directions-to-search
                                              (opponent-directions
                                                neighboring-possible))))))

;(: apply-move : Game Player Pos -> Game)
;;; return a subsequent game state given a game, player and position; if proposed
;;; move is illegal, return an error
;(define (apply-move game player pos)
;  (if (outflanks? (Game-board game) player pos)
;      (


;; ==== Visualizations ====

(: board-image : Board Integer -> Image)
;; produce an image of the board given a board and the desired width
(define (board-image board n)
  (local {
    (define play-board (square (* 8/9 n) "solid" "darkgreen"))
    (define gsq (square (/ n 9) "outline" "royalblue")) ; "grid square"
    (define bsq (overlay (square (/ n 9) "solid" "whitesmoke")
                         gsq)) ; "border square"
    (: bsq-num : Integer -> Image)
    ;; generate bsq with numbers printed on them
    (define (bsq-num num)
      (overlay (text (number->string num) (- (quotient n 9) 5) "black")
               bsq))
    (define top-border (beside (bsq-num 0) (bsq-num 1) (bsq-num 2) (bsq-num 3)
                               (bsq-num 4) (bsq-num 5) (bsq-num 6) (bsq-num 7)))
    (define left-border (above (bsq-num 0) (bsq-num 1) (bsq-num 2) (bsq-num 3)
                               (bsq-num 4) (bsq-num 5) (bsq-num 6) (bsq-num 7)))
    (define black-piece (overlay (circle (/ n 36) "solid" "black") gsq))
    (define white-piece (overlay (circle (/ n 36) "solid" "white") gsq))
    (: playing-board-row-generator : (Listof (U Player 'none)) -> Image)
    ;; generate a row of the playing-board from a list of pieces
    (define (playing-board-row-generator squares)
      (match squares
        ['() empty-image]
        [(cons hd tl)
         (beside (match hd
                   ['none gsq]
                   ['black black-piece]
                   ['white white-piece])
                 (playing-board-row-generator tl))]))
    (: playing-board-row-compiler : (Listof (U Player 'none)) -> Image)
    ;; generate a full playing-board image from a list of pieces
    (define (playing-board-row-compiler squares)
      (match squares
        ['() empty-image]
        [(cons a
               (cons b
                     (cons c
                           (cons d
                                 (cons e
                                       (cons f
                                             (cons g
                                                   (cons h tl))))))))
          (above (playing-board-row-generator (list a b c d e f g h))
                 (playing-board-row-compiler tl))]))}
  (above (beside bsq top-border)
         (beside left-border (overlay (playing-board-row-compiler
                                       (Board-squares board))
                                      play-board)))))

;(: game-image : Game Integer -> Image)
;;; produce the image of the full game, including the board, an indication of
;;; who plays next, and each player's current score given a desired width
;(define (game-image game width)
;  (local {
;    (define board-size (* 2/3 width))
;    (define info-size (* 1/3 width))
;    (define black-turn (above (overlay (circle (/ width 36) "solid" "black")
;                                   (square (/ width 9) "outline" "royalblue")
;                                   (square (/ width 9) "solid" "darkgreen"))
;                          (text "Black's Move"
;                                (quotient width 9)
;                                "black")))
;    (define white-turn (above (overlay (circle (/ width 36) "solid" "white")
;                                       (square (/ width 9) "outline" "royalblue")
;                                       (square (/ width 9) "solid" "darkgreen"))
;                              (text "White's Move"
;                                    (quotient width 9)
;                                    "black")))}
;  (above (if (equal? (Game-next game) 'black) black-turn white-turn)
;         



;; evaluation

;; === correctness ===

;; new-game                          4/ 4
;; board-ref                         4/ 4

;; outflanks?                        12/12
;; flips                             12/12
;; apply-move                        0/12

;; game-over?                        0/12
;; outcome                           0/ 8

;; board-image                       12/12
;; game-image                        2/12

;; === style ===

;; code layout                       8/ 8
;; identifiers are well named        8/ 8
;; program decomposition (helpers)   10/10

;; contracts (types)                 8/ 8
;; well-written purposes             8/ 8
;; adequate tests                    0/ 8

;; clarity (clear logic)             10/10

;; svn used correctly                2/ 2

;; _total-score_                    100/150

;; graded by ORLOVA
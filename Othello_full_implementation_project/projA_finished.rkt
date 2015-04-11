#lang typed/racket
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

;; THIS VERSION HEAVILY BORROWS CODE FROM THE SOLUTIONS FOR THE SAKE OF HAVING
;; A WORKING VERSION TO DO THE PART B


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

;; possible contents of a square on the board
(define-type Cell (U Player 'none))

;; direction
(define-struct Dir
  ([dr : (U -1 0 1)]
   [dc : (U -1 0 1)])
  #:transparent)

;; outcome of a game
(define-type Outcome (U Player 'tie))

;; maybe type
(define-type (Maybe A) (U 'Nothing (Just A)))
(define-struct (A) Just ([value : A]) #:transparent)

;; pairs
(define-struct (A B) Pair ([fst : A] [snd : B]) #:transparent)


;; ==== Constants ====

;; index offsets for the 8-neighborhood of a square
(define neighbor-offsets
  (list (Dir -1 -1) (Dir -1 0) (Dir -1 1)
        (Dir 0 -1) (Dir 0 1)
        (Dir 1 -1) (Dir 1 0) (Dir 1 1)))

;; width of the board in squares
(define board-wid 8)

;; number of squares on the board
(define num-cells (* board-wid board-wid))


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

;(: pos+ : Pos Pos -> Pos)
;;; [AUX] add two positions
;(define (pos+ pos1 pos2)
;  (make-Pos (+ (Pos-row pos1) (Pos-row pos2))
;            (+ (Pos-col pos1) (Pos-col pos2))))

(: pos+ : Pos Dir -> Pos)
;; add two positions
(define (pos+ p dir)
  (match* (p dir) [((Pos pr pc) (Dir dr dc)) (make-Pos (+ pr dr) (+ pc dc))]))

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

;; -----------------------------------------------------------------------------
; Additional stuff from solutions

(: other-player : Player -> Player)
;; return the other player
(define (other-player player)
  (match player ['white 'black] ['black 'white]))

(: same-player : Player -> (Cell -> Boolean))
;; curried function for testing if a cell holds a player's piece
(define (same-player player)
  (lambda ([p : Cell]) (symbol=? player p)))

(: is-empty? : Cell -> Boolean)
;; is a cell value 'none?
(define (is-empty? cell) (symbol=? cell 'none))

(: pos->index : Pos -> Integer)
;; convert a position to an index (0..63)
(define (pos->index p)
  (match p [(Pos row col) (+ (* board-wid row) col)]))

(: index->pos : Integer -> Pos)
;; convert an index to a position
(define (index->pos idx)
  (Pos (quotient idx 8) (remainder idx 8)))

(: cells-update : (Listof Cell) Integer Cell -> (Listof Cell))
;; functional update an element of a list of cells
(define (cells-update cells idx v)
  (local
    {(: update : Integer (Listof Cell) -> (Listof Cell))
     (define (update i cells)
       (match* (i cells)
         [(0 (cons hd tl)) (cons v (rest cells))]
         [(i (cons hd tl)) (cons hd (update (- i 1) tl))]
         [(_ _) (error 'cells-update "invalid index")]))}
    (update idx cells)))

(: board-update : Pos Cell Board -> Board)
;; functional update of a board
(define (board-update p v brd)
  (match brd [(Board cells) (make-Board (cells-update cells (pos->index p) v))]))

(: cell-at : Board Integer Integer -> Cell)
;; return the cell value at the given row and column
(define (cell-at brd r c)
  (match brd [(Board cells) (list-ref cells (+ (* board-wid r) c))]))

(: count-pieces : Board Player -> Integer)
;; count the pieces on the board belonging to the player
(define (count-pieces brd player)
  (foldl
   (lambda ([cell : Cell] [sum : Integer])
     (if (symbol=? cell player) (+ sum 1) sum))
   0
   (Board-squares brd)))
;; -----------------------------------------------------------------------------

(: try-flip-in-dir : Board Player Pos Dir -> (Listof Pos))
;; given a board, player, starting position, and direction, try to flip pieces
;; We assume that the initial pos is empty and on the board
(define (try-flip-in-dir brd player start dir)
  (local
    {(define p1 (pos+ start dir))
     (define is-other? (same-player (other-player player)))
     (: try-flip : Pos (Listof Pos) -> (Listof Pos))
     ;; flip opponent's pieces in direction dir until we hit one of player's
     ;; pieces.  Return the empty list if we cannot flip legally
     (define (try-flip p ps)
       (cond
         [(not (on-board? p)) '()]
         [(is-other? (board-ref brd p)) (try-flip (pos+ p dir) (cons p ps))]
         [(is-empty? (board-ref brd p)) '()]
         [else ps]))}
    (try-flip p1 '())))

(: flips : Board Player Pos -> (Listof Pos))
;; return a list of the squares that are flipped if the player places a piece
;; at the given position.  The result does _not_ include the played piece.
;; The empty list is returned if the move is not legal.
(define (flips brd player p)
  (local
    {(: f : Dir (Listof Pos) -> (Listof Pos))
     (define (f dir cells) (append (try-flip-in-dir brd player p dir) cells))}
    (if (and (on-board? p) (is-empty? (board-ref brd p)))
        (foldl f '() neighbor-offsets)
        '())))

(: outflanks? : Board Player Pos -> Boolean)
;; return true if the player can leagally place a piece on the board at
;; the given location.
(define (outflanks? brd player p)
  (local
    {(: f : Dir -> Boolean)
     (define (f dir) (cons? (try-flip-in-dir brd player p dir)))}
    (and (on-board? p)
         (is-empty? (board-ref brd p))
         (ormap f neighbor-offsets))))

(: board-apply-move : Board Player Pos -> Board)
;; apply a move to board; signal an error if the move is not legal
(define (board-apply-move brd player p)
  (match (flips brd player p)
    ['() (error 'apply-move "illegal move")]
    [ps (local
          {(: apply-flip : Pos Board -> Board)
           (define (apply-flip q brd) (board-update q player brd))}
          (foldl apply-flip brd (cons p ps)))]))

(: apply-move : Game Player Pos -> Game)
;; apply a move to a game state; signal an error if the move is not legal
(define (apply-move g player pos)
  (match g
    [(Game brd next)
     (if (symbol=? next player)
         (Game (board-apply-move brd player pos) (other-player next))
         (error 'apply-move "not your turn to move"))]))

(: move-possible? : Board Player -> Boolean)
;; is it possible for the player to move on the given board?
(define (move-possible? brd player)
  (local
    {(define cells (Board-squares brd))
     (: search : Integer -> Boolean)
     ;; search the board squares looking for an empty square where the
     ;; player can make a legal move (i.e., outflank the opponent).
     (define (search idx)
       (if (< idx num-cells)
           (or (outflanks? brd player (index->pos idx)) (search (+ idx 1)))
           #f))}
    (search 0)))

(: game-over? : Game -> Boolean)
;; is the game over?
(define (game-over? g)
  (match g
    [(Game brd _) (not (or (move-possible? brd 'white)
                           (move-possible? brd 'black)))]))

(: outcome : Game -> Outcome)
;; determine the outcome of a game
(define (outcome g)
  (local
    {(define brd (Game-board g))
     (define white-score (count-pieces brd 'white))
     (define black-score (count-pieces brd 'black))}
    (cond
      [(< white-score black-score) 'black]
      [(> white-score black-score) 'white]
      [else 'tie])))


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

(: game-image : Game Integer -> Image)
;; produce the image of the full game, including the board, an indication of
;; who plays next, and each player's current score given a desired width
(define (game-image game width)
  (local {
    (define brd (Game-board game))
    (define black-turn (above (overlay (circle (/ width 36) "solid" "black")
                                       (square (/ width 9) "outline" "royalblue")
                                       (square (/ width 9) "solid" "darkgreen"))
                              (text "Black's Move"
                                    (quotient width 9)
                                    "black")))
    (define white-turn (above (overlay (circle (/ width 36) "solid" "white")
                                       (square (/ width 9) "outline" "royalblue")
                                       (square (/ width 9) "solid" "darkgreen"))
                              (text "White's Move"
                                    (quotient width 9)
                                    "black")))}
  (above (board-image brd width)
         (square 10 "solid" "white")
         (if (equal? (Game-next game) 'black) black-turn white-turn)
         (text (string-append "Black's Score : "
                              (number->string (count-pieces brd 'black)))
               (quotient width 9)
               "black")
         (text (string-append "White's Score : "
                              (number->string (count-pieces brd 'white)))
               (quotient width 9)
               "black"))))
#lang typed/racket

;; NOTICE THAT THIS COMPLETED PROJECT HEAVILY UTILIZES THE SOLUTIONS TO PART A
;; GIVEN BY THE INSTRUCTORS. THANK YOU INSTRUCTORS.

(require typed/test-engine/racket-tests)

(require/typed 2htdp/image
               [#:opaque Image image?]
               [overlay (Image * -> Image)]
               [empty-image Image]
               [circle (Real String String -> Image)]
               [square (Real String String -> Image)]
               [rectangle (Real Real String String -> Image)]
               [text (String Integer String -> Image)]
               [beside (Image * -> Image)]
               [beside/align (String Image * -> Image)]
               [above (Image * -> Image)]
               [above/align (String Image * -> Image)])

;; =============================================================================
;; ============================ Part A (Solutions) =============================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA DEFINITIONS

;; game players
(define-type Player (U 'black 'white))

;; board positions
(define-struct Pos
  ([row : Integer]  ;; an integer on the interval [0,7]
   [col : Integer]) ;; an integer on the interval [0,7]
  #:transparent)

;; possible contents of a square on the board
(define-type Cell (U Player 'none))

;; game board
(define-struct Board
  ([squares : (Listof Cell)]) ;; a list of length 64
  #:transparent)

;; game state
(define-struct Game
  ([board : Board]
   [next  : Player])
  #:transparent)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS

;; index offsets for the 8-neighborhood of a square
(define neighbor-offsets
  (list (Dir -1 -1) (Dir -1 0) (Dir -1 1)
        (Dir 0 -1) (Dir 0 1)
        (Dir 1 -1) (Dir 1 0) (Dir 1 1)))

;; width of the board in squares
(define board-wid 8)

;; number of squares on the board
(define num-cells (* board-wid board-wid))

;; lower-bound on display width of a single cell
(define minimum-cell-wid 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UTILITY FUNCTIONS

(: other-player : Player -> Player)
;; return the other player
(define (other-player player)
  (match player ['white 'black] ['black 'white]))

(: same-player : Player -> (Cell -> Boolean))
;; curried function for testing if a cell holds a player's piece
(define (same-player player)
  (lambda ([p : Cell]) (symbol=? player p)))

(: player->string : Player -> String)
;; return the name of the player
(define (player->string player)
  (match player ['black "Black"] ['white "White"]))

(: is-empty? : Cell -> Boolean)
;; is a cell value 'none?
(define (is-empty? cell) (symbol=? cell 'none))

(: on-board? : Pos -> Boolean)
;; is a position on the board?
(define (on-board? p)
  (match p [(Pos r c) (and (<= 0 r 7) (<= 0 c 7))]))

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

(: board-ref : Board Pos -> Cell)
;; return the cell value at the given position
(define (board-ref brd p)
  (match brd [(Board cells) (list-ref cells (pos->index p))]))

(: pos+ : Pos Dir -> Pos)
;; add a position and a direction
(define (pos+ p dir)
  (match* (p dir) [((Pos pr pc) (Dir dr dc)) (make-Pos (+ pr dr) (+ pc dc))]))

(: count-pieces : Board Player -> Integer)
;; count the pieces on the board belonging to the player
(define (count-pieces brd player)
  (foldl
   (lambda ([cell : Cell] [sum : Integer])
     (if (symbol=? cell player) (+ sum 1) sum))
   0
   (Board-squares brd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VISUALIZATION

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GAME LOGIC

;; initial state of the game board
(define new-board
  (board-update
   (make-Pos 3 4) 'black
   (board-update
    (make-Pos 4 3) 'black
    (board-update
     (make-Pos 3 3) 'white
     (board-update
      (make-Pos 4 4) 'white
      (make-Board (make-list num-cells 'none)))))))

;; initial state of the game
(define new-game (make-Game new-board 'black))

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
;; return true if the player can legally place a piece on the board at
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


;; =============================================================================
;; ================================== Part B ===================================

;; ==== Basic Strategy ====

(define-type Strategy (Game -> Pos))

(: first-move : Strategy)
;; return the Strategy of always choosing the first cell that is possible
(define (first-move game)
  (match game
    [(Game board next)
     (local
       {(: search : Integer -> Pos)
        ;; search the board squares looking for the first empty square where
        ;; the player can make a legal move (i.e., outflank the opponent).
        (define (search idx)
          (if (< idx num-cells)
              (if (outflanks? board next (index->pos idx))
                  (index->pos idx)
                  (search (+ idx 1)))
              (error "no legal moves remaining")))}
       (search 0))]))
;; TESTS
(check-expect (first-move new-game) (Pos 2 3))

;; ==== Human Strategy ====

(: ascii->num : Integer -> Integer)
;; convert ASCII code to a number useful in this game
(define (ascii->num code)
  (- code 48))
;; TESTS
(check-expect (ascii->num 48) 0)

(: parse-pos : String -> (U Pos String))
;; parse strings and return the position it refers to
(define (parse-pos string)
  (local
    {(define code-list (map char->integer (string->list string)))}
    (cond
      [(not (= (length code-list) 2)) string]
      [else (match code-list
              [(list row col) (make-Pos (- row 48) (- col 48))])])))
;; TESTS
(check-expect (parse-pos "fjdksalf;ds fdsafads") "fjdksalf;ds fdsafads")
(check-expect (parse-pos "77") (Pos 7 7))
(check-expect (parse-pos "78") (Pos 7 8))

(: human : Strategy)
;; request human input for Strategy
(define (human game)
  (local
    {(define input (read-line))}
    (if (eof-object? input)
        (error "EOF detected")
        (local
          {(define parsed (parse-pos input))}
          (if (string? parsed)
              (begin
                (display "input is unparseable; please try again")
                (newline)
                (human game))
              parsed)))))
;; NO TESTS

;; ==== Game Administration ====

(: play-loop : Game Strategy Strategy -> (Pair (Listof (Pair Player Pos)) Game))
;; take in the initial game, the black player's strategy, and the white player's
;; strategy, and return a Pair consisting of a list of Pairs of player with
;; positions, and the end game
;; if an illegal move is given by a computer three times, the game is ended;
;; a human, however, gets unlimited tries
(define (play-loop game strat-black strat-white)
  (local
    {(: loop : Game (Listof (Pair Player Pos)) ->
               (Pair (Listof (Pair Player Pos)) Game))
     ;; do the gaming and accumulate the list of moves (a history)
     (define (loop game acc-list)
       (local
         {(define board (Game-board game))
          (define player (Game-next game))}
         (begin (display (game-image game 300))
                (newline)
                (newline)
                (newline)
                (newline)
                (newline)
                (cond
                  [(game-over? game) (make-Pair acc-list game)]
                  [(not (move-possible? board player))
                   (loop (make-Game board (other-player player)) acc-list)]
                  [else
                   (local
                     {(: n-strikes-processor : Integer -> Pos)
                      ;; solicit for a legitimate position-move a certain
                      ;; number of times; after that number of failures
                      ;; just return an error
                      (define (n-strikes-processor counter)
                        (if (= counter 0)
                            (error "failure to follow rules of input")
                            (local
                              {(define move2test
                                 (match player
                                   ['black (strat-black game)]
                                   ['white (strat-white game)]))}
                              (cond
                                [(outflanks? board player move2test) move2test]
                                [else
                                 (begin (display "given position is not legal")
                                        (newline)
                                        (n-strikes-processor (- counter 1)))]))))
                      (define move (n-strikes-processor 3))}
                     (loop (apply-move game player move)
                           (cons (make-Pair player move) acc-list)))]))))}
    (loop game '())))
;; NO TESTS

(: play-loop-forgetful : Game Strategy Strategy -> (U Player 'tie))
;; "forgets" the history of the game; good for quick and easy console play
(define (play-loop-forgetful g b w)
  (match (play-loop g b w)
    [(Pair history game-at-end) (outcome game-at-end)]))
;; NO TESTS

(: pass-and-play : -> (Pair (Listof (Pair Player Pos)) Game))
;; allow two humans to play against each other!
(define (pass-and-play)
  (play-loop new-game human human))
;; NO TESTS

;; ==== Rules-Based Strategy ====

(: is-corner? : Pos -> Boolean)
;; given a pos, determine whether it refers to a corner position
(define (is-corner? pos)
  (match pos
    [(Pos row col) (and (= (remainder row 7) 0)
                        (= (remainder col 7) 0))]))
;; TESTS
(check-expect (is-corner? (make-Pos 0 7)) #t)
(check-expect (is-corner? (make-Pos 3 4)) #f)

(: is-noncorner-edge? : Pos -> Boolean)
;; given a pos, determine whether it refers to a non-corner edge position
(define (is-noncorner-edge? pos)
  (match pos
    [(Pos row col) (and (not (is-corner? pos))
                        (or (= (remainder row 7) 0)
                            (= (remainder col 7) 0)))]))
;; TESTS
(check-expect (is-noncorner-edge? (make-Pos 0 7)) #f)
(check-expect (is-noncorner-edge? (make-Pos 3 4)) #f)
(check-expect (is-noncorner-edge? (make-Pos 3 7)) #t)
(check-expect (is-noncorner-edge? (make-Pos 3 0)) #t)

(: is-interior? : Pos -> Boolean)
;; given a pos, determine whether it refers to an interior position
(define (is-interior? pos)
  (and (not (is-corner? pos))
       (not (is-noncorner-edge? pos))))
;; TESTS
(check-expect (is-interior? (make-Pos 0 7)) #f)
(check-expect (is-interior? (make-Pos 3 4)) #t)
(check-expect (is-interior? (make-Pos 3 7)) #f)
(check-expect (is-interior? (make-Pos 3 0)) #f)
(check-expect (is-interior? (make-Pos 4 2)) #t)
(check-expect (is-interior? (make-Pos 5 1)) #t)

(: pos-list : (Listof Pos))
;; create a list of all the positions on a board for various later purposes
(define pos-list
  (map index->pos (build-list 64 (lambda ([x : Integer]) x))))

(: playable-pos-list : Game -> (Listof Pos))
;; determine a list of playable moves from a game state
(define (playable-pos-list game)
  (local
    {(define board (Game-board game))
     (define player (Game-next game))
     (: pos-flips-pairer : Pos -> (Pair Pos (Listof Pos)))
     ;; given a position, return the position paired with a list of the pieces
     ;; it would be able to flip
     (define (pos-flips-pairer pos)
       (make-Pair pos (flips board player pos)))
     (define paired-list (map pos-flips-pairer pos-list))
     (: paired-flips-nonempty? : (Pair Pos (Listof Pos)) -> Boolean)
     ;; given a pair determine whether the flips list is nonempty
     (define (paired-flips-nonempty? pair)
       (not (empty? (Pair-snd pair))))
     (define playable-pair-list (filter paired-flips-nonempty? paired-list))
     (: pair-fst-taker : (Pair Pos (Listof Pos)) -> Pos)
     ;; given a pair give back the corresponding pos
     (define (pair-fst-taker pair)
       (Pair-fst pair))}
    (map pair-fst-taker playable-pair-list)))
;; TESTS
(check-expect (playable-pos-list new-game)
              (list (Pos 2 3) (Pos 3 2) (Pos 4 5) (Pos 5 4)))

(: immediate-tactics : Strategy)
;; rules-based strategy based on the observation that corners are most desirable,
;; non-corner edges a little less so, and interior positions least
(define (immediate-tactics game)
  (local
    {(define board (Game-board game))
     (define player (Game-next game))
     (: pos-flips-pairer : Pos -> (Pair Pos (Listof Pos)))
     ;; given a position, return the position paired with a list of the pieces
     ;; it would be able to flip
     (define (pos-flips-pairer pos)
       (make-Pair pos (flips board player pos)))
     (define paired-list (map pos-flips-pairer pos-list))
     (: paired-flips-nonempty? : (Pair Pos (Listof Pos)) -> Boolean)
     ;; given a pair determine whether the flips list is nonempty
     (define (paired-flips-nonempty? pair)
       (not (empty? (Pair-snd pair))))
     (define playable-pair-list (filter paired-flips-nonempty? paired-list))
     (: is-pair-corner? : (Pair Pos (Listof Pos)) -> Boolean)
     ;; given a pair, determine whether it refers to a corner position
     (define (is-pair-corner? pair)
       (match pair
         [(Pair pos _) (is-corner? pos)]))
     (: is-pair-noncorner-edge? : (Pair Pos (Listof Pos)) -> Boolean)
     ;; given a pair, determine whether it refers to a non-corner edge position
     (define (is-pair-noncorner-edge? pair)
       (match pair
         [(Pair pos _) (is-noncorner-edge? pos)]))
     (: is-pair-interior? : (Pair Pos (Listof Pos)) -> Boolean)
     ;; given a pair, determine whether it refers to an interior position
     (define (is-pair-interior? pair)
       (match pair
         [(Pair pos _) (is-interior? pos)]))
     (define corner-choice-list (filter is-pair-corner? playable-pair-list))
     (define edge-choice-list (filter is-pair-noncorner-edge? playable-pair-list))
     (define interior-choice-list (filter is-pair-interior? playable-pair-list))
     (: flips-length : (Pair Pos (Listof Pos)) -> Integer)
     ;; given a pair, return the length of its flips list
     (define (flips-length pair)
       (length (Pair-snd pair)))}
    (cond
      [(not (empty? corner-choice-list))
       (Pair-fst (argmax flips-length corner-choice-list))]
      [(not (empty? edge-choice-list))
       (Pair-fst (argmax flips-length edge-choice-list))]
      [(not (empty? interior-choice-list))
       (Pair-fst (argmax flips-length interior-choice-list))]
      [else
       (error "strategy should not be called if no moves are possible")])))
;; TESTS
(check-expect (immediate-tactics new-game) (Pos 2 3))

;; ==== Game Heuristics ====

(define-type Heuristic (Game -> Integer))

(: piece-counting : Heuristic)
;; number of black pieces minus number of white pieces
(define (piece-counting g)
  (local
    {(define board (Game-board g))}
    (- (count-pieces board 'black)
       (count-pieces board 'white))))
;; TESTS
(check-expect (piece-counting new-game) 0)

(: count-various-pieces : Board Player Integer Integer -> Integer)
;; given a board and player, count using weights the player's heuristic result
(define (count-various-pieces board player cornerweight edgeweight)
  (foldl
   (lambda ([pos : Pos] [cell : Cell] [sum : Integer])
     (if (symbol=? cell player)
         (cond
           [(is-corner? pos) (+ sum cornerweight)]
           [(is-noncorner-edge? pos) (+ sum edgeweight)]
           [(is-interior? pos) (+ sum 1)]
           [else (error "position not on board should not be given")])
         sum))
   0
   pos-list
   (Board-squares board)))
;; TESTS
(check-expect (count-various-pieces new-board 'black 100 50) 2)

(: prefer-edges : Integer -> Heuristic)
;; given a weight for edge pieces in terms of interior pieces, return a Heuristic
;; for counting pieces that gives more importance to having edge pieces
(define (prefer-edges weight)
  (lambda ([game : Game])
    (local
      {(define board (Game-board game))}
      (- (count-various-pieces board 'black weight weight)
         (count-various-pieces board 'white weight weight)))))
;; TESTS
(check-expect ((prefer-edges 2) new-game) 0)

(: prefer-edges-and-corners : Integer Integer -> Heuristic)
;; given weights for counting non-corner edge pieces and corner pieces, return
;; the Heuristic
(define (prefer-edges-and-corners edgeweight cornerweight)
  (lambda ([game : Game])
    (local
      {(define board (Game-board game))}
      (- (count-various-pieces board 'black cornerweight edgeweight)
         (count-various-pieces board 'white cornerweight edgeweight)))))
;; TESTS
(check-expect ((prefer-edges-and-corners 2 4) new-game) 0)

;; ==== Strategy with Minimax ====

; ------------------------------------------------------------------------------
; Some stock games for testing minimax

(define already-won-game
(make-Game
  (Board
   '(black
     black
     black
     black
     black
     black
     white
     white
     black
     black
     black
     black
     black
     black
     white
     black
     black
     white
     black
     black
     white
     black
     white
     black
     black
     black
     white
     black
     white
     white
     white
     black
     black
     black
     white
     white
     black
     white
     white
     black
     black
     black
     white
     white
     white
     black
     white
     white
     black
     black
     white
     white
     white
     white
     white
     white
     black
     black
     black
     black
     black
     black
     black
     black))
  'black))

(define almost-won-game
(make-Game
  (Board
   '(black
     black
     black
     black
     black
     black
     white
     white
     black
     black
     black
     black
     black
     black
     white
     black
     black
     white
     black
     black
     white
     black
     white
     black
     black
     black
     white
     black
     white
     white
     white
     black
     black
     black
     white
     white
     black
     white
     white
     black
     black
     black
     white
     white
     white
     black
     white
     white
     black
     black
     white
     white
     white
     white
     white
     none
     black
     black
     black
     black
     black
     black
     black
     black))
  'black))
; ------------------------------------------------------------------------------

(: minimax-eval : Heuristic Integer Game -> Integer)
;; consume a Heuristic, a ply, and a Game and assign a score using the heuristic
(define (minimax-eval heuristic ply game)
  (if (or (game-over? game) (= ply 0))
      (heuristic game)
      (local
        {(define board (Game-board game))
         (define player (Game-next game))}
        (if (not (move-possible? board player))
            (minimax-eval heuristic
                          (- ply 1)
                          (make-Game board (other-player player)))
            (local
              {(: next-game : Pos -> Game)
               ;; given a pos apply the move and return the resulting game
               (define (next-game pos)
                 (apply-move game player pos))
               (define next-game-list (map next-game (playable-pos-list game)))}
              (cond
                [(equal? player 'black)
                 (apply max (map (lambda ([next-game : Game])
                                   (minimax-eval heuristic (- ply 1) next-game))
                                 next-game-list))]
                [(equal? player 'white)
                 (apply min (map (lambda ([next-game : Game])
                                   (minimax-eval heuristic (- ply 1) next-game))
                                 next-game-list))]
                [else (error
                       "should never happen; game ends or a player plays")]))))))
;; TESTS
(check-expect (minimax-eval (prefer-edges-and-corners 2 4) 0 new-game) 0)
(check-expect (minimax-eval (prefer-edges-and-corners 2 4) 1 new-game) 3)
(check-expect (minimax-eval (prefer-edges-and-corners 2 4) 2 new-game) 0)
(check-expect (minimax-eval (prefer-edges-and-corners 2 4) 4 new-game) -2)
(check-expect (minimax-eval (prefer-edges-and-corners 2 4) 3 already-won-game)
              38)

(: minimax : Heuristic Integer -> Strategy)
;; consume the Heuristic and ply and build a Strategy
(define (minimax heuristic ply)
  (lambda ([game : Game])
    (local
      {(define board (Game-board game))
       (define player (Game-next game))
       (: pos-game-pairer : Pos -> (Pair Pos Game))
       ;; given a pos return a pair of the pos and resulting game when applied
       (define (pos-game-pairer pos)
         (make-Pair pos (apply-move game player pos)))
       (define next-pos-game-list (map pos-game-pairer
                                       (playable-pos-list game)))}
      (cond
        [(empty? next-pos-game-list)
         (error "strategy should not be called when player cannot play")]
        [(equal? player 'black)
         (Pair-fst (argmax (lambda ([pair : (Pair Pos Game)])
                             (minimax-eval heuristic ply (Pair-snd pair)))
                           next-pos-game-list))]
        [(equal? player 'white)
         (Pair-fst (argmin (lambda ([pair : (Pair Pos Game)])
                             (minimax-eval heuristic ply (Pair-snd pair)))
                           next-pos-game-list))]))))
;; TESTS
(check-expect ((minimax (prefer-edges-and-corners 2 4) 0) new-game)
              (Pos 2 3))
(check-expect ((minimax (prefer-edges-and-corners 2 4) 1) new-game)
              (Pos 2 3))
(check-expect ((minimax (prefer-edges-and-corners 2 4) 3) new-game)
              (Pos 2 3))
(check-expect ((minimax (prefer-edges-and-corners 2 4) 3) almost-won-game)
              (Pos 6 7))

;; ==== Montymax Strategy ====

(: pick-upto : (All (A) Integer (Listof A) -> (Listof A)))
;; given a number of things to have and a list of things, return the given number
;; of things in a new list chosen at random
(define (pick-upto limit init-list)
  (if (<= (length init-list) limit)
      init-list
      (take (shuffle init-list) limit)))
;; TESTS
(check-expect (length (pick-upto 3 (list 1 2 3 4 93 3))) 3)
(check-expect (length (pick-upto 2 (list "a" "a" "a" "a" "a"))) 2)
(check-expect (length (pick-upto 5 (list "a" "a" "a" "a"))) 4)

(: montymax-eval : Heuristic Integer Game Integer -> Integer)
;; consume a Heuristic, a ply, and a Game and assign a score using the heuristic
(define (montymax-eval heuristic ply game maxbranch)
  (if (or (game-over? game) (= ply 0))
      (heuristic game)
      (local
        {(define board (Game-board game))
         (define player (Game-next game))}
        (if (not (move-possible? board player))
            (montymax-eval heuristic
                           (- ply 1)
                           (make-Game board (other-player player))
                           maxbranch)
            (local
              {(: next-game : Pos -> Game)
               ;; given a pos apply the move and return the resulting game
               (define (next-game pos)
                 (apply-move game player pos))
               (define picked-pos-list
                 (pick-upto maxbranch (playable-pos-list game)))
               (define next-game-list (map next-game picked-pos-list))}
              (cond
                [(equal? player 'black)
                 (apply max (map (lambda ([next-game : Game])
                                   (montymax-eval heuristic
                                                  (- ply 1)
                                                  next-game
                                                  maxbranch))
                                 next-game-list))]
                [(equal? player 'white)
                 (apply min (map (lambda ([next-game : Game])
                                   (montymax-eval heuristic
                                                  (- ply 1)
                                                  next-game
                                                  maxbranch))
                                 next-game-list))]
                [else (error
                       "should never happen; game ends or a player plays")]))))))
;; NO TESTS

(: montymax : Heuristic Integer Integer -> Strategy)
;; given a heuristic function, the ply, and a maximum branching factor at each
;; level of the tree (discarding the rest at random), return a strategy
(define (montymax heuristic ply maxbranch)
  (lambda ([game : Game])
    (local
      {(define board (Game-board game))
       (define player (Game-next game))
       (: pos-game-pairer : Pos -> (Pair Pos Game))
       ;; given a pos return a pair of the pos and resulting game when applied
       (define (pos-game-pairer pos)
         (make-Pair pos (apply-move game player pos)))
       (define next-pos-game-list (map pos-game-pairer (playable-pos-list game)))}
      (cond
        [(empty? next-pos-game-list)
         (error "strategy should not be called when player cannot play")]
        [(equal? player 'black)
         (Pair-fst (argmax (lambda ([pair : (Pair Pos Game)])
                             (montymax-eval heuristic
                                            ply
                                            (Pair-snd pair)
                                            maxbranch))
                           next-pos-game-list))]
        [(equal? player 'white)
         (Pair-fst (argmin (lambda ([pair : (Pair Pos Game)])
                             (montymax-eval heuristic
                                            ply
                                            (Pair-snd pair)
                                            maxbranch))
                           next-pos-game-list))]))))
;; NO TESTS

(test)




;; evaluation

;; The function pair-fst-taker is just the same as Pair-fst.

;; There's a lot of duplicated code between minimax and montymax.

;; === correctness ===

;; first-move                6/ 6
;; immediate-tactics        10/10

;; human (parse-pos)        10/10
;; play-loop                10/10

;; piece-counting            6/ 6
;; prefer-edges              5/ 5
;; prefer-edges-and-corners  5/ 5

;; minimax-eval             12/12
;; minimax                  12/12

;; montymax                 12/12

;; === style ===

;; code layout                       6/ 8
;; identifiers are well named        8/ 8
;; program decomposition (helpers)  10/10

;; contracts (types)                 8/ 8
;; well-written purposes             8/ 8
;; adequate tests                    8/ 8

;; clarity (clear logic)            10/10

;; svn used correctly                2/ 2

;; _total-score_                  148/150

;; graded by Nick Seltzer
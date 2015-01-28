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
   [crop (-> Number Number Number Number Image Image)]
   [flip-vertical (-> Image Image)]
   [flip-horizontal (-> Image Image)]
   [freeze (-> Image Image)]
   [rotate (-> Number Image Image)])

;; medium-sized icons for each suit

(: hearts-med Image)
(define hearts-med 
  (crop -2 10 32 36 (text "♥" 40 "red")))

(: spades-med Image)
(define spades-med 
  (crop -2 10 32 36 (text "♠" 40 "black")))

(: clubs-med Image)
(define clubs-med 
  (crop 0 10 32 36 (text "♣" 40 "black")))

(: diamonds-med Image)
(define diamonds-med 
  (crop -2 10 32 36 (text "♦" 40 "red")))

;; small icons for each suit

(: hearts-sm Image)
(define hearts-sm 
  (crop -2 4 18 20 (text "♥" 20 "red")))

(: spades-sm Image)
(define spades-sm 
  (crop -2 4 18 20 (text "♠" 20 "black")))

(: clubs-sm Image)
(define clubs-sm 
  (crop -1 4 18 20 (text "♣" 20 "black")))

(: diamonds-sm Image)
(define diamonds-sm 
  (crop -2 4 18 20 (text "♦" 20 "red")))

;; suit structures

(define-struct Suit
  ([name : String]
   [color : String]
   [small-icon : Image]
   [medium-icon : Image])
  #:transparent)

(define hearts   (make-Suit "hearts" "red" hearts-sm hearts-med))
(define diamonds (make-Suit "diamonds" "red" diamonds-sm diamonds-med))
(define clubs    (make-Suit "clubs" "black" clubs-sm clubs-med))
(define spades   (make-Suit "spades" "black" spades-sm spades-med))

;; some useful operations

(: frame (-> Integer Image Image))
;; given padding in pixels and an image, draw a thin
;; black rectangle around the image
(define (frame padding i)
  (overlay i (rectangle (+ padding (image-width i)) 
                        (+ padding (image-height i)) 
                        "outline" 
                        "black")))

(: spacer-v (-> Number Image))
;; construct a tall, thin, white rectangle for vertical space
(define (spacer-v n)
  (rectangle 1 n "solid" "white"))

(: flip-v (-> Image Image))
;; flip an image vertically, even if image includes text
;; (this is why "freeze" is called)
(define (flip-v i)
  (flip-vertical (freeze i)))

(: mirror-v (-> Image Image))
;; "vertical mirroring" -- show image above its own reflection
(define (mirror-v i)
  (above i (flip-v i)))


;; === student's code below ===


;; ---- HELPER FUNCTIONS ----

(: center-strip-nine : Suit -> Image)
;; create helper function for generating a center "strip" for nines, since a big
;; difference between nine cards and ten cards is this center strip
(define (center-strip-nine suit)
  (above (rectangle 40 80 "solid" "white") 
         (Suit-medium-icon suit) 
         (rectangle 40 80 "solid" "white")))

(: center-strip-ten : Suit -> Image)
;; create helper function for generating a center "strip" for tens, since a big
;; difference between nine cards and ten cards is this center strip
(define (center-strip-ten suit)
  (above (rectangle 40 40 "solid" "white") 
         (Suit-medium-icon suit) 
         (rectangle 40 40 "solid" "white") 
         (flip-v (Suit-medium-icon suit)) 
         (rectangle 40 40 "solid" "white")))

(: left-side-nine : Suit -> Image)
;; create helper function for generating the left size of the nine card; right side
;; can be generated by rotating this image by 180 degrees
(define (left-side-nine suit)
  (beside (above (text "9" 20 (Suit-color suit))
                 (Suit-small-icon suit)
                 (rectangle 20 160 "solid" "white"))
          (above (rectangle 40 20 "solid" "white")
                 (Suit-medium-icon suit)
                 (Suit-medium-icon suit)
                 (flip-v (Suit-medium-icon suit))
                 (flip-v (Suit-medium-icon suit))
                 (rectangle 40 20 "solid" "white"))))

(: left-side-ten : Suit -> Image)
;; create helper function for generating the left size of the ten card; right side
;; can be generated by rotating this image by 180 degrees
(define (left-side-ten suit)
  (beside (above (text "10" 20 (Suit-color suit))
                 (Suit-small-icon suit)
                 (rectangle 20 160 "solid" "white"))
          (above (rectangle 40 20 "solid" "white")
                 (Suit-medium-icon suit)
                 (Suit-medium-icon suit)
                 (flip-v (Suit-medium-icon suit))
                 (flip-v (Suit-medium-icon suit))
                 (rectangle 40 20 "solid" "white"))))


;; ---- MAIN FUNCTIONS ----

(: nine-of (-> Suit Image))
;; generate an image of a "nine-of" playing card based on a selection of suit
(define (nine-of suit)
  (frame 5 (beside (left-side-nine suit)
                   (center-strip-nine suit)
                   (rotate 180 (left-side-nine suit)))))

(: ten-of (-> Suit Image))
;; generate an image of a "ten-of" playing card based on a selection of suit
(define (ten-of suit)
  (frame 5 (beside (left-side-ten suit)
                   (center-strip-ten suit)
                   (rotate 180 (left-side-ten suit)))))
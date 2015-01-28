;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
9
(+ 9 1)
(+ 1 9)
(- 9 1)
(- 1 9)

(* 9 1)
(* 1 9)
(/ 9 1)
(/ 1 9)

(expt 9 1)
(expt 9 2)
(expt 1 9)
(expt 2 9)
(+ (expt 9 1) 1)
(+ (expt 9 2) 2)
(- (expt 9 2) (expt 9 1))

(= (expt 9 1) (expt 9 2))
(< (expt 9 1) (expt 9 2))
(> (expt 9 1) (expt 9 2))

;; a : Integer
(define a 7)
;; b : Integer
(define b 8)
(* a b)
(- (* b b) (* a a))
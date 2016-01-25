#lang plai-typed

(require (typed-in racket
                   [pi : number]))

;; Eric Tran - Assignment 1 (Captain Teach)

;; Exercise 3.3.3.
;; Develop area-cylinder.
;; The program consumes the radius of the cylinder's base disk and its height.
;; Its result is the surface area of the cylinder.
(define (area-cylinder [r : number] [h : number]) : number
  (+ (* (* (* 2 pi) r) h) (* (* (* 2 pi) r) r))
  )

;; test cases for area-cylinder
(test (area-cylinder 2 4) 75.4)
(test (area-cylinder 5 5) 314.16)

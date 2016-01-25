#lang plai-typed

(+ 3 4)

(test (* 4 13) 52)

true

false

(test (or false true) true)

(define (add-nums [a : number] [b : number]) : number
  (+ a b))

(add-nums 5 6)

(test (add-nums 5 6) 11)

;; Dollar -> euros consumes a number of dollars and produces the euro equivalent
(define (dollar->euros [toConvert : number]) : number 
  (* toConvert 0.92))

(test (dollar->euros 1) 0.92)
(test (dollar->euros 2) 1.84)


;; Sum-coins consumes four numbers, numbers of pennies, nickels, dimes, and quarters, and produces the total amount 
(define (sum-coins [p : number] [n : number] [d : number] [q : number]) : number
  (+(+ (* d 0.10) (* q 0.25)) (+ (* p 0.01) (* n 0.05)))
  )

(test (sum-coins 1 2 3 4) 1.41)

;; Interest produces the actual amount of interest that the money earns in a year.
;; The bank pays a flat 4% for deposits of up to $1,000,
;; a flat 4.5% per year for deposits of up to $5,000,
;; and a flat 5% for deposits of more than $5,000
(define (interest [deposit : number]) : number
  (cond
    [(<= deposit 1000) (* 0.04 deposit)]
    [(<= deposit 5000) (* 0.045 deposit)]
    [(> deposit 5000) (* 0.05 deposit)]
    )
  )

(test (interest 10000) 500)
(test (interest 2500) 112.5)
(test (interest 500) 20)

;; How-many, which consumes the coefficients a, b, and c of a proper quadratic equation
;; and determines how many solutions the equation has
(define (how-many [a : number] [b : number] [c : number]) : number
  (cond
    
    [(> (* b b) (* 4 (* a c))) 2]
    [(= (* b b) (* 4 (* a c))) 1]
    [(< (* b b) (* 4 (* a c))) 0]
    )
  )

(test (how-many 1 0 -9) 2)
(test (how-many 2 4 2) 1)
(test (how-many 1 0 4) 0)

;; What-kind consumes the coeffeiceints a, b, and c and determines whether the question is degenerate or not
;; If it isn't degenerate, it will return how many solutions there are
(define (what-kind [a : number] [b : number] [c : number]) : symbol

  (cond
    [(= a 0) 'degenerate]
    [(> a 0) 
     (cond
    
       [(> (* b b) (* 4 (* a c))) 'two]
       [(= (* b b) (* 4 (* a c))) 'one]
       [(< (* b b) (* 4 (* a c))) 'none]
       )
     ]
    )
  )

(test (what-kind 0 1 2) 'degenerate)
(test (what-kind 1 0 -9) 'two)
(test (what-kind 2 4 2) 'one)
(test (what-kind 1 0 4) 'none)

;; Defining a type office furniture (abstract class) with two concrete class, desk & bookshelf
(define-type OfficeFurniture
  [desk (width : number) (height : number) (depth : number)]
  [bookshelf (depth : number) (numShelves : number) (shelfWidth : number)]
  )

;; Created a desk instance
(define myDesk (desk 4 8 2))

;; Consumes an office furniture, checks its type, and returns the footprint accordingly
(define (furniture-footprint [officeFurn : OfficeFurniture]) : number
  (type-case OfficeFurniture officeFurn
    [desk (w h d) (* w d)]
    [bookshelf (d n s) (* d s)]
    )
  )

(test (furniture-footprint myDesk) 8)

;; Created a bookshelf instance 
(define myBookShelf (bookshelf 1 2 3))

(test (furniture-footprint myBookShelf) 3)

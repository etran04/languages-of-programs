#lang plai-typed

(require (typed-in racket
                   [pi : number]))

;; Eric Tran - Assignment 1

;; Exercise 2.3.3
;; An old-style movie theater has a simple profit function.
;; Each customer pays $5 per ticket. Every performance costs the theater $20, plus $.50 per attendee.
;; Develop the function total-profit. It consumes the number of attendees (of a show) and produces how much income the attendees produce.
(define (total-profit [people : number]) : number
  (- (* people 5) (+ 20 (* people 0.5)))
  )

;; test case for total-profit
(test (total-profit 4) -2)
(test (total-profit 10) 25)

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

;; 2.2 Magic Tricks
; Represent a magic trick
(define-type Trick
  [card-trick (decks : number) (volunteers : number)]
  [guillotine (realism : number) (has-blood? : boolean)])

;; trick-minutes computes how long a trick will take.
;; Assume that a card trick requires one minute per deck of cards and that its length is doubled for every volunteer required,
;; and that a guillotine trick requires ten minutes unless it has blood, in which case it requires 20.
(define (trick-minutes [trick : Trick]) : number
  (type-case Trick trick
    [card-trick (d v) (doubleLen d v)]
    [guillotine (r b) (cond
                        [b 20]
                        [else 10]
                        )]
    )
  )

;; helper function for trick-minutes
(define (doubleLen [d : number] [v : number]) : number
  (cond
    [(= 0 v) d]
    [else (* 2 (doubleLen d (- v 1)))]
    )
  )

;; test cases for trick-minutes (card-trick)
(test (trick-minutes (card-trick 1 2)) 4)
(test (trick-minutes (card-trick 5 5)) 160)

;; test cases for trick-minutes (guillotine)
(test (trick-minutes (guillotine 5 #t)) 20)
(test (trick-minutes (guillotine 5 #f)) 10)

;; 2.3 Low-degree Polynomials
;; data definition for a polynomial 
(define-type Polynomial
  [linear (a : number) (b : number)]
  [quadratic (a : number) (b : number) (c : number)]
  )

;; eval accepts a polynomial and a value for the variable,
;; produces the result of plugging in the value for the variable
(define (eval [eq : Polynomial] [x : number]) : number
  (type-case Polynomial eq
      [linear (a b) (+ (* a x) b)]
      [quadratic (a b c) (+ (+ (* (* a x) x) (* b x)) c)]
      )
  )

;; test cases for eval - linear
(test (eval (linear 0 0) 0) 0)
(test (eval (linear 1 1) 1) 2)
(test (eval (linear 0 1) 1) 1)
(test (eval (linear 5 8) 3) 23)

;; test cases for eval - quadratic
(test (eval (quadratic 0 0 0) 0) 0)
(test (eval (quadratic 1 1 1) 3) 13)
(test (eval (quadratic 2 3 2) 2) 16)
(test (eval (quadratic 2 1 9) 0) 9)
(test (eval (quadratic -1 0 -4) -1) -5)
(test (eval (quadratic -1 -2 -4) 2) -12)


;; 2.4 Derivative 
;; derivative accepts a polynomial and returns another polynomial that represents its derivative
(define (derivative [p : Polynomial]) : Polynomial
  (type-case Polynomial p
    [linear (a b) (linear 0 a)]
    [quadratic (a b c) (linear (* 2 a) b)]
    ))

;; test case for derivative - linear
(test (derivative (linear 5 2)) (linear 0 5))
(test (derivative (linear 10 0)) (linear 0 10))

;; test case for derivative - quadratic
(test (derivative (quadratic 1 2 3)) (linear 2 2))
(test (derivative (quadratic 5 3 4)) (linear 10 3))

;; 2.5 Binary Tree
;; data definition for a binary tree
(define-type BTree
  [leaf (v : symbol)]
  [node (left : BTree) (right : BTree)]
  )

;; 3 Examples of BTree
(define BTree1 (node (leaf 'left) (leaf 'right)))
(define BTree2 (node (node (leaf 'one)(leaf 'two)) (node (leaf 'three)(leaf 'four))))
(define BTree3 (leaf 'leaf1))
(define BTree4 (node (node (node (leaf 'farLeft) (leaf 'farLeftRight)) (leaf 'oneLevelAboveFarLeftRight)) (node (leaf 'rightOLAFLR) (leaf 'farRight))))

;; 2.6 Min-Depth
;; min-depth accepts a binary tree and produces the length of the shortest path to a leaf
(define (min-depth [root : BTree]) : number
  (type-case BTree root
    [leaf (v) 0]
    [node (left right) (+ 1 (minimum-two (min-depth left) (min-depth right)))]
   )
  )

;; helper function to compare two number and return the minimum
(define (minimum-two [x : number] [y : number]) : number
  (cond
    [(> x y) y]
    [else x]
    )
  )

;; test cases for min-depth
(test (min-depth BTree1) 1)
(test (min-depth BTree2) 2)
(test (min-depth BTree3) 0)
(test (min-depth BTree4) 2)


;; 2.7 Traversal
;; in-order accepts a binary tree and produces a list representing an "in-order" traversal of the binary tree
(define (in-order [root : BTree]) : (listof 'a)
  (type-case BTree root
    [leaf (v) (list v)]
    [node (left right) (combine (in-order left) (in-order right))]
   )
  )

;; helper function to add a list to another
(define (combine [one : (listof 'a)] [two : (listof 'a)]) : (listof 'a)
  (cond
    [(empty? one) two]
    [else (cons (first one) (combine (rest one) two))]
    )
  )

(test (combine (list '1 '2 '3) (list '4 '5 '6)) (list '1 '2 '3 '4 '5 '6))

;; test cases for in-order
(test (in-order BTree2) (list 'one 'two 'three 'four))
(test (in-order BTree4) (list 'farLeft 'farLeftRight 'oneLevelAboveFarLeftRight 'rightOLAFLR 'farRight))

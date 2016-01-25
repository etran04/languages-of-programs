#lang plai-typed

;; Eric Tran - Lab3

(require plai-typed/s-exp-match)

;; ArithC language
(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)])

;; interp takes in an ArithC and returns the number representing it
(define (interp [a : ArithC]) : number
  (type-case ArithC a
    [numC (n) n]
    [plusC (l r) (+ (interp l) (interp r))]
    [multC (l r) (* (interp l) (interp r))]
    )
  )

;; test cases for interp
(test (interp (numC 5)) 5)
(test (interp (plusC (numC 10) (numC 20))) 30)
(test (interp (multC (numC 5) (numC 11))) 55)

;;num-nums accepts an ArithC and returns a number indicating how many numbers it contains
(define (num-nums [a : ArithC]) : number
  (type-case ArithC a
    [numC (n) 1]
    [plusC (l r) (+ (num-nums l) (num-nums r))]
    [multC (l r) (+ (num-nums l) (num-nums r))]
    )
  )

;; test cases for num-nums
(test (num-nums (numC 20)) 1)
(test (num-nums (plusC (numC 10) (numC 15))) 2)
(test (num-nums (multC (multC (numC 2) (numC 5)) (plusC (numC 7) (numC 8)))) 4)

;; auxilary arithS definition
(define-type ArithS
  [numS (n : number)]
  [plusS (l : ArithS) (r : ArithS)]
  [minusS (l : ArithS) (r : ArithS)]
  [multS (l : ArithS) (r : ArithS)]
  [uNegateS (n : ArithS)]
  )

;; parse function (changed everything from ArithC to ArithS
(define (parse [s : s-expression]) : ArithS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-match? '(ANY + ANY) s)
         (local [(define l (s-exp->list s))]
           (plusS (parse (first l)) (parse (third l))))]
    [(s-exp-match? '(ANY * ANY) s)
         (local [(define l (s-exp->list s))]
           (multS (parse (first l)) (parse (third l))))]
    [(s-exp-match? '(ANY - ANY) s)
         (local [(define l (s-exp->list s))]
           (minusS (parse (first l)) (parse (third l))))]
    [(s-exp-match? '(- ANY) s)
         (local [(define l (s-exp->list s))]
           (uNegateS (parse (second l))))]
    [else (error 'parse "invalid input")]
    )
  )

;; test cases for parse - parsing a single number
(test (parse '5) (numS 5))
(test (parse '-2) (numS -2))

;;test cases for parse - parsing for addition
(test
 (parse `{5 + 10})
 (plusS (numS 5) (numS 10)))
(test
 (parse `{13 + 9})
 (plusS (numS 13) (numS 9)))

;; test cases for parse - parsing for multiplication
(test
 (parse `{5 * 10})
 (multS (numS 5) (numS 10)))
(test
 (parse `{3 * 12})
 (multS (numS 3) (numS 12)))

;; test cases for parse - parsing for subtraction
(test
 (parse `{5 - 10})
 (minusS (numS 5) (numS 10)))
(test
 (parse `{18 - 21})
 (minusS (numS 18) (numS 21)))

;; test cases for parse - parsing for negation
(test
 (parse `{- 20})
 (uNegateS (numS 20)))
(test
 (parse `{- 99})
 (uNegateS (numS 99)))

;; test case for error
(test/exn
 (parse '{20 25 +})
 "invalid input")
(test/exn
 (parse '{12 10 -})
 "invalid input") 

;; desugar function -> gives us an ArithC
(define (desugar [input : ArithS]) : ArithC
   (type-case ArithS input
     [numS (n) (numC n)]
     [plusS (l r) (plusC (desugar l) (desugar r))]
     [minusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
     [multS (l r) (multC (desugar l) (desugar r))]
     [uNegateS (n) (desugar (minusS (numS 0) n))]
     )
   )

;; test cases for desugar function
(test (desugar (numS 25)) (numC 25))
(test (desugar (plusS (numS 10) (numS 7))) (plusC (numC 10) (numC 7)))
(test (desugar (multS (numS 5) (numS 4))) (multC (numC 5) (numC 4)))
(test (desugar (minusS (numS 5) (numS 1))) (plusC (numC 5) (multC (numC -1) (numC 1))))
(test (desugar (uNegateS (numS 15))) (plusC (numC 0) (multC (numC -1) (numC 15))))

;; top-interp accepts an s-exp and calls the parser then interp function
(define (top-interp [s : s-expression]) : number
  (interp (desugar (parse s)))
  )

;; test cases for top-interp
(test (top-interp '{5 + 21}) 26)
(test (top-interp '{4 * 7}) 28)
(test (top-interp '{20 - 13}) 7)
(test (top-interp '{- 18}) -18)

;; doublemap consumes a function and a list and returns the list formed by applying the function
;; twice to each element of the original list
(define (doublemap [func : (number -> number)] [list : (listof number)]) : (listof number)
  (cond
    [(empty? list) list]
    [else (cons (func (func (first list)))
          (doublemap func (rest list)))
          ]
    )
  )

;; helper function to test if double map works
(define (timeSix [n : number]) : number
  (* n 6)
  )

;; another helper function to test if double map works
(define (add3 [n : number]) : number
  (+ n 3)
  )

;; test cases for doublemap
(test (doublemap timeSix (list 1 2 3)) (list 36 72 108))
(test (doublemap add3 (list 0 0)) (list 6 6))

;; zip consumes two lists of the same length and returns a new list of lists where each
;; element of the new list is a list containing both of the corresponding elements from the
;; original lists
(define (zip [one : (listof number)] [two : (listof number)]) : (listof (listof number))
  (cond
    [(empty? one) empty]
    [else (cons (cons (first one) (cons (first two) empty)) (zip (rest one) (rest two)))
          ]
    )
  )

;; test cases for zip
(test (zip (list 1 2 3) (list 4 5 6)) (list (list 1 4) (list 2 5) (list 3 6)))
(test (zip (list 9) (list 6)) (list (list 9 6)))
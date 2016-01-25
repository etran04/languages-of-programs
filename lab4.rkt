#lang plai-typed

;; Eric Tran
;; CSC 430 - Lab4

((lambda (x) (+ x 2))3)

((lambda (f g) (f (g 3)))
 (lambda (x) (+ x 3))
 (lambda (x) (* x 2)))

;; Develop the curried-add function.
;; It takes a number ’a’ and returns a function that takes a number ’b’ and returns a+b.
(define (curried-add [a : number]) : (number -> number)
  (lambda (b) (+ a b))
  )

;; test cases for curried-add
(test ((curried-add 5) 15) 20) 
(test ((curried-add 20) 10) 30)

;; curry2 takes a function of two arguments, and produces a function that takes one argument
;; and produces a function that takes one argument and produces the result of calling the input
;; function on the two given arguments
(define (curry2 [func : ('a 'b -> 'c)]) : ('a -> ('b -> 'c))
  (lambda (a) (lambda (b) (func a b)))
  )

;; helper method for testing curry2
(define (add2 [a : number] [b : number]) : number
  (+ a b)
  )

;; test cases for curry2
(test (((curry2 add2) 3) 6) 9 )
(test (((curry2 add2) -3) -6) -9 )

;; curry3 takes a function of three arguments, and produces a function that takes one argument
;; and produces a function that takes one argument and produces a function that takes one argument
;; and produces the result of calling the input function on the three given arguments
(define (curry3 [func : (number number number -> number)]) : (number -> (number -> (number -> number)))
  (lambda (c) (lambda (b) (lambda (a) (func a b c))))
  )

;; helper method for testing curry3
(define (add3 [a : number] [b : number] [c : number]) : number
  (+ (+ a b) c)
  )

;; test cases for curry3
(test ((((curry3 add3) 1) 2) 3) 6)
(test ((((curry3 add3) -1) -5) -10) -16)

;; contains? consumes a list and a symbol and returns true exactly when the symbol occurs in the list
(define (contains? [lst : (listof 'a)] [sym : 'a]) : boolean
  (cond
    [(empty? lst) false]
    [(equal? sym (first lst)) true]
    [else (contains? (rest lst) sym)]
    )
  )

;; test cases for contains?
(test (contains? (list 'hello 'can 'you 'hear 'me) 'a) #f)
(test (contains? (list 'hello 'can 'you 'hear 'me) 'can) #t)

;; in-list-many? consumes a source list of symbols and a list of query symbols, and returns
;; a list of booleans indicating for the corresponding element of the query list whether it occurs
;; in the source list
(define (in-list-many? [symLst : (listof 'a)] [queryLst : (listof 'a)]) : (listof boolean)
  (map ((curry2 contains?) symLst) queryLst)
  )

;; test cases for in-list-many?
(test (in-list-many? (list 'a 'b 'c) (list 'a 'b 'c)) (list #t #t #t))
(test (in-list-many? (list 'a 'b 'c) (list 'd 'e 'f)) (list #f #f #f))
(test (in-list-many? (list 'x 'y 'z) (list 'a 'y 'z)) (list #f #t #t))
#lang plai-typed

;; Instructions: Submit a test case for top-eval that contains two mutually recursive functions
;; named 'odd' and 'even' that each accept a nonnegative integer. The 'even' function should return
;; one for a number that is even, and zero for a number that is not.
;; The 'odd' function should return one for a number that is odd, and zero for a number that is not.
;; See Piazza for a plai-typed version.

(define (odd [n : number]) : boolean
  (cond [(= n 0) false]
        [else (even (- n 1))]))

(define (even [n : number]) : boolean
  (cond [(= n 0) true]
        [else (odd (- n 1))]))

;; ... code for top eval in here 

;; test case for odd/even
(test (top-eval '((func odd n (ifleq0 n 0 (even (- n 1))))
                  (func even n (ifleq0 n 1 (odd (- n 1))))
                  (func main (even 5)))
                0)
      )

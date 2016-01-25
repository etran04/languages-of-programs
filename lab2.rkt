#lang plai-typed

;; Rev-str-app accepts a list of strings and
;; returns a single string combining the input
;; strings in reverse order
(define (rev-str-app [aList : (listof string)]) : string
  (cond
    [(empty? aList) ""]
    [else (string-append
           (rev-str-app (rest aList))
           (first aList)
           )]
    )
  )

;; test cases for rev-str-app
(test (rev-str-app empty) "")
(test (rev-str-app (list "ball" "juice" "frog")) "frogjuiceball")

;; processor types
(define-type processor
  [intel (intelNum : number)]
  [amd (amdNum : number)]
  [arm (armNum : number)]
  )

;; onlyIntels consumes a list of processors and returns a list containing only the Intels
(define (onlyIntels [aList : (listof processor)]) : (listof processor)
  (cond
    [(empty? aList) empty]
    [else (type-case processor (first aList)
            [intel (num) (cons (first aList) (onlyIntels (rest aList)))]
            [amd (num) (onlyIntels (rest aList))]
            [arm (num) (onlyIntels (rest aList))]
            )]
    )
  )

;; test cases for onlyIntels
(test (onlyIntels (list (amd 1))) empty)
(test (onlyIntels (list (amd 1) (intel 1) (intel 2))) (list (intel 1) (intel 2)))
(test (onlyIntels (list (arm 1))) empty)

;; onlyAMDs consumes a list of processors and returns a list containing only the Intels
(define (onlyAMDs [aList : (listof processor)]) : (listof processor)
  (cond
    [(empty? aList) empty]
    [else (type-case processor (first aList)
            [amd (num) (cons (first aList) (onlyAMDs (rest aList)))]
            [intel (num) (onlyAMDs (rest aList))]
            [arm (num) (onlyAMDs (rest aList))]
            )]
    )
  )

;; test cases for onlyIntels
(test (onlyAMDs (list (amd 1))) (list (amd 1)))
(test (onlyAMDs (list (amd 1) (amd 2)(intel 1) (intel 2))) (list (amd 1) (amd 2)))
(test (onlyAMDs (list (arm 1))) empty)

;; onlyTheses consumes a list of processors and a particular processor predicate and
;; returns a list containing only those elements
(define (onlyThese [aList : (listof processor)]
                   [f : (processor -> boolean)]) : (listof processor)
  (cond
    [(empty? aList) empty]
    [(f (first aList)) (cons (first aList) (onlyThese (rest aList) f))]
    [else (onlyThese (rest aList) f)]
    )
  )

;; test cases for onlyTheses
(test (onlyThese (list (intel 1) (arm 1) (amd 1)) intel?) (list (intel 1)))
(test (onlyThese (list (intel 1) (arm 1) (amd 1)) amd?) (list (amd 1)))
(test (onlyThese (list (intel 1) (arm 1) (amd 1)) arm?) (list (arm 1)))

;; my-append consumes two lists and returns the result of appending the 2nd list to the first
(define (my-append[firstList : (listof 'a)]
                   [secondList : (listof 'a)]) : (listof 'a)
  (cond
    [(empty? firstList) secondList]
    [else (cons (first firstList) (my-append (rest firstList) secondList))]
    )
  )

;; test case for my-append
(test (my-append (list "a" "b" "c") (list "d" "e" "f")) (list "a" "b" "c" "d" "e" "f"))

;; my-drop consumes a list and a number and returns the list after that number
(define (my-drop [aList : (listof 'a)] [n : number]) : (listof 'a)
  (cond
    [(<= n 0) aList]
    [(empty? aList) empty]
    [else (my-drop (rest aList) (- n 1))]
    )
  )

;; test case for my-drop
(test (my-drop (list 1 2 3 4 5) 6) empty) 
(test (my-drop (list 1 2 3 4 5) 2) (list 3 4 5)) 
(test (my-drop (list empty) 1) empty)
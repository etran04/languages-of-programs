#lang plai-typed

(require plai-typed/s-exp-match)

;; Eric Tran (etran04)
;; CSC 430 - Assignment 2

;; --- 2.1 Binary arithmetic operators --- ;;
;; binOp added to be supportive of multiple different arithmetic operators

;; --- 2.4 Conditionals ---- ;;
;; ifleq0 construct added to support conditionals
;; current support: (if 'test value <= 0, do 'then, else do 'other)

;; ExprC datatype
(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : symbol) (arg : ExprC)]
  [binOp (op : symbol) (l : ExprC) (r : ExprC)]
  [ifleq0 (test : ExprC) (then : ExprC) (other : ExprC)]
  )

;; FundefC datatype
(define-type FundefC
  [func (name : symbol) (arg : symbol) (body : ExprC)]
  )

; takes an s-expression and returns as an ExprC representation
(define (parse [s : s-expression]) : ExprC
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-symbol? s) (idC (s-exp->symbol s))]
    [(s-exp-match? '(NUMBER) s)
     (local [(define l (s-exp->list s))]
       (parse (first l)))]
    [(s-exp-match? '(SYMBOL) s)
     (local [(define l (s-exp->list s))]
       (parse (first l)))] 
    [(s-exp-match? '(ifleq0 ANY ANY ANY) s)
     (local [(define l (s-exp->list s))]
       (ifleq0 (parse (second l)) (parse (third l)) (parse (fourth l))))]
    [(s-exp-match? '(+ ANY ANY) s)
     (local [(define l (s-exp->list s))]
       (binOp '+ (parse (second l)) (parse (third l))))]
    [(s-exp-match? '(- ANY ANY) s)
     (local [(define l (s-exp->list s))]
       (binOp '- (parse (second l)) (parse (third l))))]
    [(s-exp-match? '(* ANY ANY) s)
     (local [(define l (s-exp->list s))]
       (binOp '* (parse (second l)) (parse (third l))))]
    [(s-exp-match? '(/ ANY ANY) s)
     (local [(define l (s-exp->list s))]
       (binOp '/ (parse (second l)) (parse (third l))))]
    [else (error 'parse "invalid input")]
    )
  )

;; table function for mapping operator names to actual racket procedures
(define (getOperator [check : symbol]) : (number number -> number)
  [cond
    [(equal? check '+) +]
    [(equal? check '-) -]
    [(equal? check '*) *]
    [(equal? check '/) /]
    [else (error 'getOperator "unimplemented")]
    ]
  )

;; --- 2.2 Functions with 0 or more arguments --- ;;

;; takes an s-expression function, and parses it as a FundefC
;; currently only parses a function with one argument
(define (parse-fundef [s : s-expression]) : FundefC
  [cond
    ;; one argument parsing for fundefC
    [(s-exp-match? '(SYMBOL ANY ANY ANY) s)
     (local [(define l (s-exp->list s))]
       (func (s-exp->symbol (second l))
             (s-exp->symbol (third l))
             (parse (fourth l))))]
    ;; zero argument parsing for fundefC
    [(s-exp-match? '(SYMBOL ANY ANY) s)
     (local [(define l (s-exp->list s))]
       (func (s-exp->symbol (second l))
             '_
             (parse (third l))))]
    [else (error 'parse-fundef "unimplemented")]])



;; takes an s-expression and parses it as a list of FundefC
(define (parse-prog [s : s-expression]) : (listof FundefC)
  [cond 
    [(s-exp-match? '(SYMBOL ANY ANY ANY) s) (cons (parse-fundef s) empty)]
    [(s-exp-list? s)
       (map parse-fundef (s-exp->list s))]])

;; helper function for looking up the function definition in a list (got from book)
(define (get-fundef [name : symbol] [fds : (listof FundefC)]) : FundefC
  (cond
    [(empty? fds) (error 'get-fundef "reference to undefined function")]
    [(cons? fds) (cond
                   [(equal? name (func-name (first fds))) (first fds)]
                   [else (get-fundef name (rest fds))])]))


;; --- 2.3 Main --- ;;
;; "Evaluating a program means applying the 'main' function"

;; interprets function named main from the fundefs, and interprets its body 
(define (interp-fns [funs : (listof FundefC)]) : number
  [type-case FundefC (get-fundef 'main funs)
    [func (n a b) (interp b funs)]]
  )

;; substitution of replacing a name in an expr with another expr (got most from book, modified for binOp and ifleq0)
;; what = what we want to replace the name with
;; for = what name we want to perform substitution
;; in = in which expression we want to do it
(define (subst [what : ExprC] [for : symbol] [in : ExprC]) : ExprC
  (type-case ExprC in
  [numC (n) in]
  [idC (s) (cond
             [(symbol=? s for) what]
             [else in])]
  [appC (f a) (appC f (subst what for a))]
  [binOp (s l r) (binOp s (subst what for l) (subst what for r))]
  [ifleq0 (test then other) (ifleq0 (subst what for test)
                                   (subst what for then)
                                   (subst what for other))]
    )
  )


;; interprets the given expression, using the list of funcs to resolve the application
(define (interp [exp : ExprC] [funs : (listof FundefC)]) : number
  (type-case ExprC exp
    [numC (n) n]
    [idC (_) (error 'interp "shouldn't get here")]
    [appC (f a)
          (local ([define fd (get-fundef f funs)])
              (interp (subst a
                             (func-arg fd)
                             (func-body fd))
                      funs))]
    [binOp (s l r) ((getOperator s) (interp l funs) (interp r funs))]
    [ifleq0 (test then other) [cond
                                [(>= 0 (interp test funs)) (interp then funs)]
                                [else (interp other funs)]
                              ]]
    )
  )

;; combines parsing and evaluation
;;(define (top-eval [fun-sexps : s-expression])  : number
;;  (interp-fns (parse-prog fun-sexps))
;;  )

;; ------------------ TEST CASES ------------------------- ;;

;; test cases for parse 
(test (parse '(7)) (numC 7))
(test (parse '(ifleq0 7 8 9))
      (ifleq0 (numC 7) (numC 8) (numC 9)))
(test (parse '(aSymbol))
      (idC 'aSymbol))
(test/exn (parse '('throw 'error))
          "invalid input")
(test (parse '(+ 0 7))
      (binOp '+ (numC 0) (numC 7)))
(test (parse '(- 9 6))
      (binOp '- (numC 9) (numC 6)))
(test (parse '(* 5 10))
      (binOp '* (numC 5) (numC 10)))
(test (parse '(/ 9 3))
      (binOp '/ (numC 9) (numC 3)))

;; test cases for parse-fundef
(test (parse-fundef '(newFn myFn x (+ x 1)))
      (func 'myFn 'x (binOp '+ (idC 'x) (numC 1))))
(test (parse-fundef '(newFn otherFn y (* y 5)))
      (func 'otherFn 'y (binOp '* (idC 'y) (numC 5))))
(test/exn (parse-fundef '(newFn myFn x y (+ x 1)))
      "unimplemented")
(test (parse-fundef '(newFn zeroArgFn (+ x 5)))
      (func 'zeroArgFn '_ (binOp '+ (idC 'x) (numC 5))))

;; test cases for get-fundef
(test (get-fundef 'fnA (list (func 'fnA 'anArg (numC 8)))) (func 'fnA 'anArg (numC 8)))
(test/exn (get-fundef 'fnZ (list (func 'fnA 'anArg (numC 8))
                             (func 'fnB 'anotherOne (numC 10))
                             (func 'fnC 'oneMore (numC 12)))) "reference to undefined function")

;; test cases for getOperator
(test (getOperator '+) +)
(test (getOperator '-) -)
(test (getOperator '*) *)
(test (getOperator '/) /)
(test/exn (getOperator '%) "unimplemented")

;; test case for subst
(test (subst (numC 7) 'a (binOp '* (idC 'a) (numC 5)))
      (binOp '* (numC 7) (numC 5)))
(test (subst (numC 7) 'a (binOp '* (idC 'b) (numC 5)))
      (binOp '* (idC 'b) (numC 5)))
(test (subst (numC 7) 'a (ifleq0 (binOp '+ (idC 'a) (numC 8)) (numC 0) (numC 1)))
      (ifleq0 (binOp '+ (numC 7) (numC 8)) (numC 0) (numC 1)))
(test (subst (numC 7) 'a (appC 'fnA (idC 'a)))
      (appC 'fnA (numC 7)))

;; test cases for interp
(test (interp (numC 20)
              (list (func 'fnA 'var (binOp '+ (idC 'var) (numC 10)))))
      20)
(test (interp (binOp '/ (numC 5) (numC 10))
              (list (func 'fnA 'var (binOp '+ (idC 'var) (numC 10)))))
      0.5)
(test (interp (ifleq0 (numC 5) (numC 10) (numC 20))
              (list (func 'fnA 'var (binOp '+ (idC 'var) (numC 10)))))
      20)
(test (interp (ifleq0 (numC -1) (numC 10) (numC 20))
              (list (func 'fnA 'var (binOp '+ (idC 'var) (numC 10)))))
      10)
(test/exn (interp (idC 'hello)
              (list (func 'fnA 'var (binOp '+ (idC 'var) (numC 10)))))
      "shouldn't get here")

(test (interp (appC 'fnA (numC 20))
              (list (func 'fnA 'var (binOp '+ (idC 'var) (numC 10)))))
      30)

;; test cases for parse-prog
;;(test (parse-prog '((newFn fnOne x (+ x 5)
;;                    (newFn fnTwo y (- y 5)))))
;;      (list (func 'fnOne 'x (binOp '+ (idC 'x) (numC 5)))
;;            (func 'fnTwo 'y (binOp '- (idC 'y) (numC 5)))))

;; test cases for interp-fns
(test (interp-fns (list (func 'f 'x (binOp '+ (idC 'x) (numC 0)))
                        (func 'main 'arg (numC 0))))
      0)

;;(test (interp-fns
;;       (parse-prog '{{func {f x} {+ x 0}}
;;                     {func {main} {f 1}}}))
;;      3)

;; (test (interp-fns
;;        (parse-prog '{{func {f} 5}
;;                      {func {main} {+ {f} {f}}}}))
;;       10)
;; (test/exn (interp-fns
;;            (parse-prog '{{func {f x y} {+ x y}}
;;                          {func {main} {f 1}}}))
;;           "wrong arity")

;; test cases for top-eval
;;(test (top-eval '{main {* 5 10}}) 50)
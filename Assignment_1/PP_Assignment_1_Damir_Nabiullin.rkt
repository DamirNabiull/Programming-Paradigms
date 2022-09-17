#lang slideshow
; HELPERS
; first-element? - a predicate that checks if an expression is list and has at least one element.
(define (first-element? expr)
  (cond
    [(and (list? expr) (not (empty? expr))) #t]
    [else #f]))

; second-element? - a predicate that checks if an expression is list and has at least two elements.
(define (second-element? expr)
  (cond
    [(and (list? expr) (>= (length expr) 2)) #t]
    [else #f]))



; ********************************************        SUBTASK 1        ******************************************************************************

; variable? - a predicate that checks if an expression is variable.
(define (variable? expr)
  (cond
    [(and (symbol? expr)
          (not (equal? expr '+))
          (not (equal? expr '*))) #t]
    [else #f]))

; sum? - a predicate that checks if an expression is sum.
(define (sum? expr)
  (cond
    [(second-element? expr) (equal? '+ (first expr))]
    [else #f]))

; summand-1 - a function that return first summand of the expression.
(define (summand-1 expr)
  (cond
    [(sum? expr) (second expr)]
    [else (error "Expected a sum expression of the form '(+ <expr> ...), but got: " expr)]))

; summand-2 - a function that return second summand of the expression.
(define (summand-2 expr)
  (cond
    [(and (sum? expr) (>= (length expr) 3)) (third expr)]
    [else (error "Expected a sum expression of the form '(+ <expr> <expr> ...), but got: " expr)]))

; product? - a predicate that checks if an expression is product.
(define (product? expr)
  (cond
    [(second-element? expr) (and (equal? '* (first expr)) (>= (length expr) 3))]
    [else #f]))

; multiplier-1 - a function that return first multiplier of the expression.
(define (multiplier-1 expr)
  (cond
    [(product? expr) (second expr)]
    [else (error "Expected a product expression of the form '(* <expr> ...), but got: " expr)]))

; multiplier-2 - a function that return second multiplier of the expression.
(define (multiplier-2 expr)
  (cond
    [(and (product? expr) (>= (length expr) 3)) (third expr)]
    [else (error "Expected a product expression of the form '(* <expr> <expr> ...), but got: " expr)]))


; ********************************************        SUBTASK 2        ******************************************************************************
; ***********************      !!! The complete derivative function is represented in SUBTASK 7      ************************************************

; DESCRIPTION
; Here you can see the solution for only 2 functions (sum and product) and 2 arguments accepted by this functions.

; unit? - a predicate that checks if an expression is variable or number.
(define (unit? val)
  (or (variable? val) (number? val)))

; unit-derivative - a function that is used to find a derivative of a variable or a number with respect to resp.
(define (unit-derivative val resp)
  (cond
    [(and (variable? val)(equal? val resp)) 1]
    [(unit? val) 0]
    [else (error "Expected an unit (a variable or a number), but got: " val)]))

; derivative-old - a recursive function that computes a symbolic derivative of a given expression with respect to a given variable.
(define (derivative-old expr resp)
  (cond
    [(not (variable? resp)) (error "Expected that derivative computes with respect to variable, but got: " resp)]
    [(unit? expr) (unit-derivative expr resp)]
    [(sum? expr) (list '+
                       (derivative-old (summand-1 expr) resp)
                       (derivative-old (summand-2 expr) resp))]
    [(product? expr) (list '+
                           (list '* (derivative-old (multiplier-1 expr) resp)
                                    (multiplier-2 expr))
                           (list '* (multiplier-1 expr)
                                 (derivative-old (multiplier-2 expr) resp)))]
    [else (error "Expected an expression of the form '(<operation> <expr> <expr> ...) or <variable> or <number>, but got: " expr)]))

; ********************************************        SUBTASK 3        ******************************************************************************
(define (simplify-root-sum a b)
  (cond
    [(and (number? a) (number? b)) (+ a b)]
    [(equal? a 0) b]
    [(equal? b 0) a]
    [(and (unit? a) (unit? b)) (list '+ a b)]
    [else (error "Error simplify-root-sum")]))

; ********************************************        SUBTASK 5        ******************************************************************************

; DESCRIPTION
; Here you can see the to-infix function.
; I made this function in such way that it works for exponentiation from SUBTASK 6 and for unlimited number of arguments.

; infix-add-symbol - a function that is part of to-infix function.
; This function implements the insertion of an operation symbols between the arguments of expressions.
(define (infix-add-symbol expr symbol)
  (define (helper)
    (foldl (lambda (x current)
             (append (list (to-infix x)
                           symbol)
                     current))
           empty
           (rest expr)))
  (cond
    [(or (sum? expr) (product? expr) (exp? expr)) (rest (reverse (helper)))]
    [else (error "Expected [a sum] or [a product] or [an exponentiation] expression of the form '(+ <expr> <expr> ...) or '(* <expr> <expr> ...) or '(^ <expr> <expr>) respectively, but got: " expr)]))

; to-infix - a recursive function that infix an operation symbols.
(define (to-infix expr)
  (cond
    [(unit? expr) expr]
    [(exp? expr) (infix-add-symbol expr '^)]
    [(math-func? expr) expr]
    [(sum? expr) (infix-add-symbol expr '+)]
    [(product? expr) (infix-add-symbol expr '*)]
    [else (error "Expected a sum or a product expression of the form '(+ <expr> <expr> ...) or '(* <expr> <expr> ...) respectively, but got: " expr)]))


; ********************************************        SUBTASK 6        ******************************************************************************
; ***********************      !!! The complete derivative and simplify functions are represented in SUBTASK 7      ************************************************

; DESCRIPTION
; In this SUBTASK I made helper functions for later use in SUBTASK 7
; This functions are necessary to implement derivatives of exponentiation, cos, sin, tan, and log. 

; exp? - a predicate that checks if an expression is exponentiation.
(define (exp? expr)
  (cond
    [(first-element? expr) (and (equal? '^ (first expr)) (equal? (length expr) 3))]
    [else #f]))

; sin? - a predicate that checks if an expression is sin.
(define (sin? expr)
  (cond
    [(first-element? expr) (and (equal? 'sin (first expr)) (equal? (length expr) 2))]
    [else #f]))

; cos? - a predicate that checks if an expression is cos.
(define (cos? expr)
  (cond
    [(first-element? expr) (and (equal? 'cos (first expr)) (equal? (length expr) 2))]
    [else #f]))

; tan? - a predicate that checks if an expression is tan.
(define (tan? expr)
  (cond
    [(first-element? expr) (and (equal? 'tan (first expr)) (equal? (length expr) 2))]
    [else #f]))

; log? - a predicate that checks if an expression is log.
(define (log? expr)
  (cond
    [(first-element? expr) (and (equal? 'log (first expr)) (equal? (length expr) 2))]
    [else #f]))

; math-func? - a predicate that checks if an expression is exponentiation, cos, sin, tan, or log.
(define (math-func? expr)
  (or (exp? expr)
      (sin? expr)
      (cos? expr)
      (tan? expr)
      (log? expr)))

; log-derivative - a function that calculates a derivative for log using derivative function.
(define (log-derivative expr resp)
  (cond
    [(log? expr) (list '* (list '^ (second expr) -1) (derivative (second expr) resp))]
    [else (error "Expected a log expression of the form '(log <expr>), but got: " expr)]))

; exp-derivative - a function that calculates a derivative for exponentiation using derivative function.
(define (exp-derivative expr resp)
  (cond
    [(exp? expr) (list '* (list '^ (second expr) (third expr)) (derivative (list '* (list 'log (second expr)) (third expr)) resp))]
    [else (error "Expected an exponentiation expression of the form '(^ <expr> <expr>), but got: " expr)]))

; sin-derivative - a function that calculates a derivative for sin using derivative function.
(define (sin-derivative expr resp)
  (cond
    [(sin? expr) (list '* (list 'cos (second expr)) (derivative (second expr) resp))]
    [else (error "Expected a sin expression of the form '(sin <expr>), but got: " expr)]))

; cos-derivative - a function that calculates a derivative for cos using derivative function.
(define (cos-derivative expr resp)
  (cond
    [(cos? expr) (list '* '-1 (list 'sin (second expr)) (derivative (second expr) resp))]
    [else (error "Expected a cos expression of the form '(cos <expr>), but got: " expr)]))

; tan-derivative - a function that calculates a derivative for tan using derivative function.
(define (tan-derivative expr resp)
  (cond
    [(tan? expr) (list '* (list '^ (list 'cos (second expr)) -2)(derivative (second expr) resp))]
    [else (error "Expected a tan expression of the form '(tan <expr>), but got: " expr)]))

; ********************************************        SUBTASK 7        ******************************************************************************

; convert-multiplication - a function that helps to differentiate product using derivative function.
(define (convert-multiplication expr resp)
  (define (helper prev expr ans)
    (cond
      [(empty? expr) (cons '+ ans)]
      [else (helper (append prev (list (first expr)))
                    (rest expr)
                    (append ans
                            (list (append (cons '* prev)
                                          (append (list (derivative (first expr) resp))
                                                  (rest expr))))))]))
  (cond
    [(product? expr) (helper empty (rest expr) empty)]
    [else (error "Expected a product expression of the form '(* <expr> ...), but got: " expr)]))

; convert-sum - a function that helps to differentiate sum using derivative function.
(define (convert-sum expr resp)
  (cond
    [(sum? expr) (cons '+ (map (lambda (x)
                                 (derivative x resp))
                               (rest expr)))]
    [else (error "Expected a sum expression of the form '(+ <expr> ...), but got: " expr)]))

; derivative - a recursive function that differentiate given expression with respect to resp.
(define (derivative expr resp)
  (cond
    [(not (variable? resp)) (error "Expected that derivative computes with respect to variable, but got: " resp)]
    [(unit? expr) (unit-derivative expr resp)]
    [(sum? expr) (convert-sum expr resp)]
    [(product? expr) (convert-multiplication expr resp)]
    [(exp? expr) (exp-derivative expr resp)]
    [(sin? expr) (sin-derivative expr resp)]
    [(cos? expr) (cos-derivative expr resp)]
    [(tan? expr) (tan-derivative expr resp)]
    [(log? expr) (log-derivative expr resp)]
    [else (error "Expected an expression of the form '(<operation> <expr> <expr> ...) or <variable> or <number>, but got: " expr)]))





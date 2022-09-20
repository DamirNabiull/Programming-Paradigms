#lang slideshow
; Damir Nabiullin 20.09.2022

; HELPERS

; first-element? - a predicate that checks if an expression is list and has at least one element.
(define (first-element? expr)
  (cond
    [(and (list? expr)
          (not (empty? expr)))
     #t]
    [else #f]))

; second-element? - a predicate that checks if an expression is list and has at least two elements.
(define (second-element? expr)
  (cond
    [(and (list? expr)
          (>= (length expr)
              2))
     #t]
    [else #f]))

; third-element? - a predicate that checks if an expression is list and has at least three elements.
(define (third-element? expr)
  (cond
    [(and (list? expr)
          (>= (length expr)
              3))
     #t]
    [else #f]))

; math-func? - a predicate that checks if an expression is sum, product, exponentiation, cos, sin, tan, or log.
(define (math-func? expr)
  (or (exp? expr)
      (sin? expr)
      (cos? expr)
      (tan? expr)
      (log? expr)
      (sum? expr)
      (product? expr)))

; acceptable-value? - a predicate that checks if an expression is acceptable by task description.
(define (acceptable-value? expr)
  (cond
    [(and (not (list? expr))
          (unit? expr))
     #t]
    [(and (list? expr)
          (math-func? expr))
     #t]
    [else (error "Got an unacceptable value: "
                 expr)]))

; acceptable-values? - a predicate that checks if an expression has values and expressions in acceptable form.
(define (acceptable-values? expr)
  (cond
    [(list? expr)
     (andmap acceptable-value?
             (rest expr))]
    [else (error "Expected a list, but got: "
                 expr)]))


; ********************************************        SUBTASK 1        ******************************************************************************

; variable? - a predicate that checks if an expression is variable.
(define (variable? expr)
  (and (symbol? expr)
       (not (equal? expr '+))
       (not (equal? expr '*))
       (not (equal? expr '^))
       (not (equal? expr 'sin))
       (not (equal? expr 'cos))
       (not (equal? expr 'log))
       (not (equal? expr 'tan))))

; sum? - a predicate that checks if an expression is sum.
(define (sum? expr)
  (and (second-element? expr)
       (acceptable-values? expr)
       (equal? '+ (first expr))))

; summand-1 - a function that return first summand of the expression.
(define (summand-1 expr)
  (cond
    [(sum? expr) (second expr)]
    [else (error "Expected a sum expression of the form '(+ <expr> ...), but got: "
                 expr)]))

; summand-2 - a function that return second summand of the expression.
(define (summand-2 expr)
  (cond
    [(and (sum? expr)
          (third-element? expr))
     (third expr)]
    [else (error "Expected a sum expression of the form'(+ <expr> <expr> ...), but got: "
                 expr)]))

; product? - a predicate that checks if an expression is product.
(define (product? expr)
  (and (second-element? expr)
       (acceptable-values? expr)
       (equal? '* (first expr))))

; multiplier-1 - a function that return first multiplier of the expression.
(define (multiplier-1 expr)
  (cond
    [(product? expr) (second expr)]
    [else (error "Expected a product expression of the form '(* <expr> ...), but got: "
                 expr)]))

; multiplier-2 - a function that return second multiplier of the expression.
(define (multiplier-2 expr)
  (cond
    [(and (product? expr)
          (third-element? expr))
     (third expr)]
    [else (error "Expected a product expression of the form '(* <expr> <expr> ...), but got: "
                 expr)]))


; ********************************************        SUBTASK 2        ******************************************************************************
; ***********************      !!! The complete derivative function is represented in SUBTASK 7      ************************************************

; unit? - a predicate that checks if an expression is variable or number.
(define (unit? val)
  (or (variable? val)
      (number? val)))

; unit-derivative - a function that is used to find a derivative of a variable or a number with respect to resp.
(define (unit-derivative val resp)
  (cond
    [(and (variable? val)
          (equal? val resp))
     1]
    [(unit? val) 0]
    [else (error "Expected an unit (a variable or a number), but got: "
                 val)]))

; derivative-old - a recursive function that computes a symbolic derivative of a given expression with respect to a given variable.
(define (derivative-old expr resp)
  (cond
    [(not (variable? resp)) (error "Expected that derivative computes with respect to variable, but got: "
                                   resp)]
    [(unit? expr) (unit-derivative expr
                                   resp)]
    [(sum? expr) (list '+
                       (derivative-old (summand-1 expr)
                                       resp)
                       (derivative-old (summand-2 expr)
                                       resp))]
    [(product? expr) (list '+
                           (list '*
                                 (derivative-old (multiplier-1 expr)
                                                 resp)
                                 (multiplier-2 expr))
                           (list '*
                                 (multiplier-1 expr)
                                 (derivative-old (multiplier-2 expr)
                                                 resp)))]
    [else (error "Expected an expression of the form '(<operation> <expr> <expr> ...) or <variable> or <number>, but got: "
                 expr)]))


; ********************************************        SUBTASK 3        ******************************************************************************
; ***********************      !!! The simplify function is represented in SUBTASK 7      ***********************************************************

; leaf? - a predicate that checks if an expression is leaf and only consists of numbers or variables.
(define (leaf? expr)
  (andmap (lambda (x)
            (unit? x))
          (rest expr)))

; combiner - a function that combines symbol and variables to expression or to a single variable.
(define (combiner args symbol)
  (cond
    [(equal? (length args)
             1)
     (first args)]
    [else (cons symbol
                args)]))

; combine-nums - a function that combines numbers of the expression expr to a list with one number or to an empty list.
(define (combine-nums expr)
  (cond
    [(empty? (filter number?
                     (rest expr)))
     empty]
    [(sum? expr)
     (list (apply +
                  (filter number?
                          (rest expr))))]
    [(product? expr)
     (list (apply *
                  (filter number?
                          (rest expr))))]
    [else (error "Expected a sum or product expression of the form '(<operation> <expr> ...), but got: "
                 expr)]))

; get-vars - a function that gets all arguments of expression except numbers.
(define (get-vars expr)
  (cond
    [(or (sum? expr)
         (product? expr))
     (filter (lambda (x)
               (not (number? x)))
             (rest expr))]
    [else (error "Expected a sum or product expression of the form '(<operation> <expr> ...), but got: "
                 expr)]))

; simplify-at-root - a function that simplifies expression only with numbers and variables.
(define (simplify-at-root expr)
  (cond
    [(and (not (or (sum? expr)
                   (product? expr)))
          (math-func? expr))
     expr]
    [(not (or (sum? expr)
              (product? expr)))
     (error "Expected a sum or product expression of the form '(<operation> <expr> ...), but got: " expr)]
    [else ((lambda (nums vars)
             (cond
               [(empty? nums)(combiner vars
                                       (first expr))]
               [(empty? vars) (first nums)]
               [(equal? (first nums) 0)
                (cond
                  [(sum? expr) (combiner vars
                                         (first expr))]
                  [(product? expr) 0])]
               [(and (equal? (first nums)
                             1)
                     (product? expr))
                (combiner vars
                          (first expr))]
               [else
                (cons (first expr)
                      (append nums
                              vars))]))
           (combine-nums expr)
           (get-vars expr))]))


; ********************************************        SUBTASK 5        ******************************************************************************

; infix-add-symbol - a function that implements the insertion of an operation symbols between the arguments of expressions.
(define (infix-add-symbol expr symbol)
  (define (helper)
    (foldl (lambda (x current)
             (append (list (to-infix x)
                           symbol)
                     current))
           empty
           (rest expr)))
  (cond
    [(or (sum? expr)
         (product? expr)
         (exp? expr))
     (rest (reverse (helper)))]
    [else
     (error "Expected [a sum] or [a product] or [an exponentiation] expression of the form '(+ <expr> <expr> ...) or '(* <expr> <expr> ...) or '(^ <expr> <expr>) respectively, but got: "
            expr)]))

; to-infix - a recursive function that infix an operation symbols.
(define (to-infix expr)
  (cond
    [(unit? expr) expr]
    [(exp? expr)
     (infix-add-symbol expr
                       '^)]
    [(math-func? expr) expr]
    [(sum? expr)
     (infix-add-symbol expr
                       '+)]
    [(product? expr)
     (infix-add-symbol expr
                       '*)]
    [else
     (error "Expected a sum or a product expression of the form '(+ <expr> <expr> ...) or '(* <expr> <expr> ...) respectively, but got: "
            expr)]))


; ********************************************        SUBTASK 6        ******************************************************************************
; ***********************      !!! The complete derivative and simplify functions are represented in SUBTASK 7      ************************************************

; exp? - a predicate that checks if an expression is exponentiation.
(define (exp? expr)
  (cond
    [(and (equal? (length expr)
                  3)
          (acceptable-values? expr))
     (equal? '^
             (first expr))]
    [else #f]))

; sin? - a predicate that checks if an expression is sin.
(define (sin? expr)
  (cond
    [(and (equal? (length expr)
                  2)
          (acceptable-values? expr))
     (equal? 'sin
             (first expr))]
    [else #f]))

; cos? - a predicate that checks if an expression is cos.
(define (cos? expr)
  (cond
    [(and (equal? (length expr)
                  2)
          (acceptable-values? expr))
     (equal? 'cos
             (first expr))]
    [else #f]))

; tan? - a predicate that checks if an expression is tan.
(define (tan? expr)
  (cond
    [(and (equal? (length expr)
                  2)
          (acceptable-values? expr))
     (equal? 'tan
             (first expr))]
    [else #f]))

; log? - a predicate that checks if an expression is log.
(define (log? expr)
  (cond
    [(and (equal? (length expr)
                  2)
          (acceptable-values? expr))
     (equal? 'log
             (first expr))]
    [else #f]))

; log-derivative - a function that calculates a derivative for log using derivative function.
(define (log-derivative expr resp)
  (cond
    [(log? expr)
     (list '*
           (list '^
                 (second expr)
                 -1)
           (derivative (second expr)
                       resp))]
    [else
     (error "Expected a log expression of the form '(log <expr>), but got: "
            expr)]))

; exp-derivative - a function that calculates a derivative for exponentiation using derivative function.
(define (exp-derivative expr resp)
  (cond
    [(exp? expr)
     (list '*
           (list '^
                 (second expr)
                 (third expr))
           (derivative (list '*
                             (list 'log
                                   (second expr))
                             (third expr))
                       resp))]
    [else
     (error "Expected an exponentiation expression of the form '(^ <expr> <expr>), but got: "
            expr)]))

; sin-derivative - a function that calculates a derivative for sin using derivative function.
(define (sin-derivative expr resp)
  (cond
    [(sin? expr)
     (list '*
           (list 'cos
                 (second expr))
           (derivative (second expr)
                       resp))]
    [else
     (error "Expected a sin expression of the form '(sin <expr>), but got: "
            expr)]))

; cos-derivative - a function that calculates a derivative for cos using derivative function.
(define (cos-derivative expr resp)
  (cond
    [(cos? expr)
     (list '*
           '-1
           (list 'sin
                 (second expr))
           (derivative (second expr)
                       resp))]
    [else
     (error "Expected a cos expression of the form '(cos <expr>), but got: "
            expr)]))

; tan-derivative - a function that calculates a derivative for tan using derivative function.
(define (tan-derivative expr resp)
  (cond
    [(tan? expr)
     (list '*
           (list '^
                 (list 'cos
                       (second expr))
                 -2)
           (derivative (second expr)
                       resp))]
    [else
     (error "Expected a tan expression of the form '(tan <expr>), but got: "
            expr)]))


; ********************************************        SUBTASK 7        ******************************************************************************

; convert-multiplication - a function that helps to differentiate product using derivative function.
(define (convert-multiplication expr resp)
  (define (helper prev expr ans)
    (cond
      [(empty? expr) (cons '+ ans)]
      [else (helper (append prev
                            (list (first expr)))
                    (rest expr)
                    (append ans
                            (list (append (cons '*
                                                prev)
                                          (append (list (derivative (first expr)
                                                                    resp))
                                                  (rest expr))))))]))
  (cond
    [(product? expr)
     (helper empty
             (rest expr)
             empty)]
    [else
     (error "Expected a product expression of the form '(* <expr> ...), but got: "
            expr)]))

; convert-sum - a function that helps to differentiate sum using derivative function.
(define (convert-sum expr resp)
  (cond
    [(sum? expr)
     (cons '+
           (map (lambda (x)
                  (derivative x resp))
                (rest expr)))]
    [else
     (error "Expected a sum expression of the form '(+ <expr> ...), but got: "
            expr)]))

; derivative - a recursive function that differentiate given expression with respect to resp.
(define (derivative expr resp)
  (cond
    [(not (variable? resp))
     (error "Expected that derivative computes with respect to variable, but got: "
            resp)]
    [(unit? expr) (unit-derivative expr
                                   resp)]
    [(sum? expr) (convert-sum expr
                              resp)]
    [(product? expr) (convert-multiplication expr
                                             resp)]
    [(exp? expr) (exp-derivative expr
                                 resp)]
    [(sin? expr) (sin-derivative expr
                                 resp)]
    [(cos? expr) (cos-derivative expr
                                 resp)]
    [(tan? expr) (tan-derivative expr
                                 resp)]
    [(log? expr) (log-derivative expr
                                 resp)]
    [else
     (error "Expected an expression of the form '(<operation> <expr> <expr> ...) or <variable> or <number>, but got: "
            expr)]))

; simplify - a recursive function that simplifies given expression.
(define (simplify expr)
  (cond
    [(unit? expr) expr]
    [(math-func? expr)
     (cond
       [(leaf? expr)
        (simplify-at-root expr)]
       [else
        (simplify-at-root (cons (first expr)
                                (map simplify
                                     (rest expr))))])]
    [else
     (error "Expected an available expression or a value, but got: "
            expr)]))


; ********************************************        SUBTASK 8        ******************************************************************************

; insert - a recursive function that represents insertion sort (insert val to ans).
(define (insert val ans)
  (cond
    [(list? val) (foldl insert
                        ans
                        val)]
    [(variable? val)
     (cond
       [(empty? ans) (list val)]
       [(ormap (lambda (x)
                 (equal? val x))
               ans)
        ans]
       [(symbol<? val
                  (first ans))
        (cons val ans)]
       [else (cons (first ans)
                   (insert val
                           (rest ans)))])]
    [else ans]))

; variables-of - a function that returns sorted list of distinct variables used in a given expression.
(define (variables-of expr)
  (foldl insert empty expr))


; ********************************************        SUBTASK 9        ******************************************************************************

; gradient - a function that calculates a gradient of a multivariable expression.
(define (gradient expr resp)
  (map (lambda (x)
         (simplify (derivative expr
                               x)))
       resp))
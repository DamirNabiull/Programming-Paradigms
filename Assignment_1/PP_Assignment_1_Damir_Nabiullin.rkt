#lang slideshow
; HELPERS
(define (first-element? expr)
  (cond
    [(and (list? expr) (not (empty? expr))) #t]
    [else #f]))

(define (second-element? expr)
  (cond
    [(and (list? expr) (>= (length expr) 2)) #t]
    [else #f]))

; TASK 1
; SUBTASK 1
(define (variable? expr)
  (cond
    [(and (symbol? expr)
          (not (equal? expr '+))
          (not (equal? expr '*))) #t]
    [else #f]))

(define (sum? expr)
  (cond
    [(second-element? expr) (equal? '+ (first expr))]
    [else #f]))

(define (summand-1 expr)
  (cond
    [(sum? expr) (second expr)]
    [else (error "Expected a sum expression of the form '(+ <expr> ...), but got: " expr)]))

(define (summand-2 expr)
  (cond
    [(and (sum? expr) (>= (length expr) 3)) (third expr)]
    [else (error "Expected a sum expression of the form '(+ <expr> <expr> ...), but got: " expr)]))

(define (product? expr)
  (cond
    [(second-element? expr) (and (equal? '* (first expr)) (>= (length expr) 3))]
    [else #f]))

(define (multiplier-1 expr)
  (cond
    [(product? expr) (second expr)]
    [else (error "Expected a product expression of the form '(* <expr> ...), but got: " expr)]))

(define (multiplier-2 expr)
  (cond
    [(and (product? expr) (>= (length expr) 3)) (third expr)]
    [else (error "Expected a product expression of the form '(* <expr> <expr> ...), but got: " expr)]))



; SUBTASK 2
(define (is-unit? val)
  (or (variable? val) (number? val)))

(define (unit-derivative val resp)
  (cond
    [(and (variable? val)(equal? val resp)) 1]
    [(is-unit? val) 0]
    [else (error "Expected an unit (a variable or a number), but got: " val)]))

; check if resp is variable else error
(define (derivative-old expr resp)
  (cond
    [(not (variable? resp)) (error "Expected that derivative computes with respect to variable, but got: " resp)]
    [(is-unit? expr) (unit-derivative expr resp)]
    [(sum? expr) (list '+
                       (derivative-old (summand-1 expr) resp)
                       (derivative-old (summand-2 expr) resp))]
    [(product? expr) (list '+
                           (list '* (derivative-old (multiplier-1 expr) resp)
                                    (multiplier-2 expr))
                           (list '* (multiplier-1 expr)
                                 (derivative-old (multiplier-2 expr) resp)))]
    [else (error "Expected an expression of the form '(<operation> <expr> <expr> ...) or <variable> or <number>, but got: " expr)]))

; SUBTASK 3
(define (simplify-root-sum a b)
  (cond
    [(and (number? a) (number? b)) (+ a b)]
    [(equal? a 0) b]
    [(equal? b 0) a]
    [(and (is-unit? a) (is-unit? b)) (list '+ a b)]
    [else (error "Error simplify-root-sum")]))

;(define (simplify-at-root expr)
 ; (cond))


; SUBTASK 6
(define (exp? expr)
  (cond
    [(first-element? expr) (and (equal? '^ (first expr)) (equal? (length expr) 3))]
    [else #f]))

(define (sin? expr)
  (cond
    [(first-element? expr) (and (equal? 'sin (first expr)) (equal? (length expr) 2))]
    [else #f]))

(define (cos? expr)
  (cond
    [(first-element? expr) (and (equal? 'cos (first expr)) (equal? (length expr) 2))]
    [else #f]))

(define (tan? expr)
  (cond
    [(first-element? expr) (and (equal? 'tan (first expr)) (equal? (length expr) 2))]
    [else #f]))

(define (log? expr)
  (cond
    [(first-element? expr) (and (equal? 'log (first expr)) (equal? (length expr) 2))]
    [else #f]))

(define (log-derivative expr resp derivative-func)
  (cond
    [(log? expr) (list '* (list '^ (second expr) -1) (derivative-func (second expr) resp))]
    [else (error "Expected a log expression of the form '(log <expr>), but got: " expr)]))

(define (exp-derivative expr resp derivative-func)
  (cond
    [(exp? expr) (list '* (list '^ (second expr) (third expr)) (derivative-func (list '* (list 'log (second expr)) (third expr)) resp))]
    [else (error "Expected an exponentiation expression of the form '(^ <expr> <expr>), but got: " expr)]))

(define (sin-derivative expr resp derivative-func)
  (cond
    [(sin? expr) (list '* (list 'cos (second expr)) (derivative-func (second expr) resp))]
    [else (error "Expected a sin expression of the form '(sin <expr>), but got: " expr)]))

(define (cos-derivative expr resp derivative-func)
  (cond
    [(cos? expr) (list '* '-1 (list 'sin (second expr)) (derivative-func (second expr) resp))]
    [else (error "Expected a cos expression of the form '(cos <expr>), but got: " expr)]))

(define (tan-derivative expr resp derivative-func)
  (cond
    [(tan? expr) (list '* (list '^ (list 'cos (second expr)) -2)(derivative-func (second expr) resp))]
    [else (error "Expected a tan expression of the form '(tan <expr>), but got: " expr)]))

; SUBTASK 7
(define (convert-multiplication expr resp derivative-func)
  (define (helper prev expr ans)
    (cond
      [(empty? expr) (cons '+ ans)]
      [else (helper (append prev (list (first expr)))
                    (rest expr)
                    (append ans
                            (list (append (cons '* prev)
                                          (append (list (derivative-func (first expr) resp))
                                                  (rest expr))))))]))
  (cond
    [(product? expr) (helper empty (rest expr) empty)]
    [else (error "Expected a product expression of the form '(* <expr> ...), but got: " expr)]))

(define (convert-sum expr resp derivative-func)
  (cond
    [(sum? expr) (cons '+ (map (lambda (x)
                                 (derivative-func x resp))
                               (rest expr)))]
    [else (error "Expected a sum expression of the form '(+ <expr> ...), but got: " expr)]))

(define (derivative expr resp)
  (cond
    [(not (variable? resp)) (error "Expected that derivative computes with respect to variable, but got: " resp)]
    [(is-unit? expr) (unit-derivative expr resp)]
    [(sum? expr) (convert-sum expr resp derivative)]
    [(product? expr) (convert-multiplication expr resp derivative)]
    [(exp? expr) (exp-derivative expr resp derivative)]
    [(sin? expr) (sin-derivative expr resp derivative)]
    [(cos? expr) (cos-derivative expr resp derivative)]
    [(tan? expr) (tan-derivative expr resp derivative)]
    [(log? expr) (log-derivative expr resp derivative)]
    [else (error "Expected an expression of the form '(<operation> <expr> <expr> ...) or <variable> or <number>, but got: " expr)]))





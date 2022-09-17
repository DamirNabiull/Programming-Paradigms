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
    [(is-unit? val) 0]))
; add error checking

; check if resp is variable else error
(define (derivative expr resp)
  (cond
    [(is-unit? expr) (unit-derivative expr resp)]
    [(sum? expr) (list '+
                       (derivative (summand-1 expr) resp)
                       (derivative (summand-2 expr) resp))]
    [(product? expr) (list '+
                           (list '* (derivative (multiplier-1 expr) resp)
                                    (multiplier-2 expr))
                           (list '* (multiplier-1 expr)
                                 (derivative (multiplier-2 expr) resp)))]))

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



; SUBTASK 7
; ADD COS SIN etc.
(define (convert-multiplication expr resp der-func)
  (define (helper prev expr ans)
    (cond
      [(empty? expr) (cons '+ ans)]
      [else (helper (append prev (list (first expr)))
                    (rest expr)
                    (append ans
                            (list (append (cons '* prev)
                                          (append (list (der-func (first expr) resp))
                                                  (rest expr))))))]))
  (helper empty (rest expr) empty))

(define (convert-sum expr resp derivative-func)
  (cons '+
        (map (lambda (x)
               (derivative-func x resp))
             (rest expr))))

(define (derivative-new expr resp)
  (cond
    [(empty? expr) empty]
    [(is-unit? expr) (unit-derivative expr resp)]
    [(sum? expr) (convert-sum expr resp derivative-new)]
    [(product? expr) (convert-multiplication expr resp derivative-new)]))





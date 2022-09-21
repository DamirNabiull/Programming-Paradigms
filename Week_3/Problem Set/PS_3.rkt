#lang slideshow
; Problem Set 3
; Damir Nabiullin 21.09.2022

; _____ Task 1 _____
; _____ A

(define (binary-product x pair)
  (cons (+ (car pair)
           (* (cdr pair)
              x))
        (* 2
           (cdr pair))))

(define (binary-to-decimal bits)
  (car (foldl binary-product
              (cons 0 1)
              (reverse bits))))

(binary-to-decimal '(1 0 1 1 0))

; _____ B

(define (add-if-flag x pair)
  (cond
    [(or (cdr pair)
         (equal? x 1))
     (cons (append (car pair)
                   (list x))
           #t)]
    [else pair]))

(define (remove-leading-zeros bits)
  (car (foldl add-if-flag (cons empty #f) bits)))

(remove-leading-zeros '(0 0 0 1 0 1 1 0))

; _____ C

(define (inc-if-zero x cur)
  (cond
    [(equal? x 0) (+ cur 1)]
    [else cur]))

(define (count-zeros bits)
  (foldl inc-if-zero 0 (remove-leading-zeros bits)))

(count-zeros '(0 0 0 1 0 1 1 0))

; _____ D

(define (combine x pair)
  (cond
    [(empty? (cdr pair)) (cons (list (list x)) (list x))]
    [(equal? (car (cdr pair)) x) (cons (append (list (append (list x) (first (car pair)))) (rest (car pair))) (list x))]
    [else (cons (append (list (list x)) (car pair)) (list x))]))

(define (group-consecutive bits)
  (reverse (car (foldl combine (cons empty empty) bits))))

(group-consecutive '(0 0 0 1 0 1 1 0))

; _____ E

(define (encode-with-lengths bits)
  (map length
       (group-consecutive (remove-leading-zeros bits))))

; _____ F

(define (list-of-n x pair)
  (cons (append (car pair)
                (map (lambda (x) (cdr pair)) (range x)))
        (cond
          [(equal? (cdr pair) 1) 0]
          [else 1])))

(define (decode-with-lengths encoded)
  (car (foldl list-of-n (cons empty 1) encoded)))

; _____ Task 2 _____

(define employees
  '(("John" "Malkovich" . 29)
    ("Anna" "Petrova" . 22)
    ("Ivan" "Ivanov" . 23)
    ("Anna" "Karenina" . 40)))

; _____ A
(define (fullname employee)
  (cons (car (first employees))
        (car (cdr (first employees)))))
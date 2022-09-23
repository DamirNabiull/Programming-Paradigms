#lang slideshow
; Damir Nabiullin 23.09.2022
; 

; Problem set 4

; _____ Task 1
; _____ A

(define (replicate n item)
  (cond
    [(< n 1) empty]
    [(equal? n 1) (list item)]
    [else (append (list item) (replicate (- n 1) item))]))

(replicate 10 'a)
(replicate 3 '(1 . 2))

; _____ B

; This function returns pair of list without dot between items
; as was described on lecture: (cons (list 1) (list 1 2)) -> (list (list 1) 2 3)
(define (split item n)
  (cond
    [(or (equal? n 0)
         (empty? item))
     (cons empty (list item))]
    [else (cons (append (list (first item))
                        (car (split (rest item)
                                    (- n 1))))
                (cdr (split (rest item)
                            (- n 1))))]))

(split '(1 2 3 4 5) 2)
(split '(a b c d) 4)
(split '(a b c) 4)
(split '(a b c) 0)

; _____ C

(define (chunks item n)
  (cond
    [(<= (length item) n) (list item)]
    [else (append (list (car (split item n)))
                  (chunks (second (split item n)) n))]))

(chunks '(1 2 3 4 5) 2)
(chunks '(a b c d e f) 3)

; _____ D

(define (windows item n)
  (cond
    [(< (length item) n) empty]
    [(= (length item) n) (list item)]
    [else (append (list (car (chunks item n)))
                  (windows (rest item) n))]))

(windows '(1 2 3 4 5) 2)
(windows '(a b c d e) 3)

; _____ Task 2
; _____ A

(define (make-pairs x cur)
  (cond
    [(empty? (second cur)) (first cur)]
    [else (list (append (first cur)
                        (map (lambda (y)
                               (cons x y))
                             (second cur)))
                (rest (second cur)))]))

; This function returns pair of list without dot between items
; as was described on lecture: (cons (list 1) (list 1 2)) -> (list (list 1) 2 3)
(define (pairs item)
  (foldl make-pairs (list empty (rest item)) item))

(pairs '(a b c d))

; _____ B

(define (split-by-n item)
  (lambda (x) (split item x)))

; This function uses split function. Therefore there is no dots between items.
(define (splits item)
  (map (split-by-n item) (range (+ (length item) 1))))

(splits '(a b c))

; _____ C

(define (op-to-pair op pair)
  (op (car pair) (cdr pair)))

(define (max-product item)
  (foldl (lambda (x cur) (cond
                       [(> (op-to-pair * x)
                           (op-to-pair * cur))
                        x]
                       [else cur]))
         (first (pairs item))
         (pairs item)))

(max-product '(1 2 3 4 3 2 1))

; _____ D

(define (max-binary-op op item)
  (foldl (lambda (x cur) (cond
                       [(> (op-to-pair op x)
                           (op-to-pair op cur))
                        x]
                       [else cur]))
         (first (pairs item))
         (pairs item)))

(max-binary-op * '(1 2 3 4 3 2 1))
(max-binary-op - '(1 2 3 4 3 2 1))

; _____ E

; I have discussed how to solve this task with Anna Dluzhinskaya

(define (equal-len n)
  (lambda (x) (equal? (length x) n)))

(define (combine-lists x)
  (lambda (y cur-2)
    (cond
      [(list? y)
       (append cur-2
               (list (append y
                             (list x))))]
      [else
       (append cur-2
               (list (append (list y)
                             (list x))))])))

(define (append-answers x cur)
  (append cur
          (list (list x))
          (foldl (combine-lists x)
                 empty
                 cur)))

(define (combinations item n)
  (filter (equal-len n)
          (foldl append-answers
                 empty
                 item)))
 
(combinations '(a b c d) 3)

; _____ Task 3
; _____ A

(define (max item)
  (foldl (lambda (x cur)
           (cond
             [(> x cur) x]
             [else cur]))
         (first item) item))

(max '(1 5 3 6 2 0))

; _____ B

(define (second-max item)
  (cdr (foldl (lambda (x cur)
                (cond
                  [(> x (car cur))
                   (cons x (car cur))]
                  [(> x (cdr cur))
                   (cons (car cur) x)]
                  [else cur]))
              (cons (first item)
                    (first item))
              item)))

(second-max '(1 5 3 6 2 0))

; _____ C

(define (top-3 item)
  (foldl (lambda (x cur)
           (cond
             [(> x (first cur))
              (list x (first cur) (second cur))]
             [(> x (second cur))
              (list (first cur) x (second cur))]
             [(> x (third cur))
              (list (first cur) (second cur) x)]
             [else cur]))
         (list (first item)
               (first item)
               (first item))
         item))

(top-3 '(5 3 6 2 8 1 0))

; _____ D

(define (combine x pair)
  (cond
    [(empty? (cdr pair))
     (cons (list (list x))
           (list x))]
    [(equal? (car (cdr pair)) x)
     (cons (append (list (append (list x)
                                 (first (car pair))))
                   (rest (car pair)))
           (list x))]
    [else (cons (append (list (list x))
                        (car pair))
                (list x))]))

(define (group item)
  (reverse (car (foldl combine
                       (cons empty
                             empty)
                       item))))

(group '(a b b c c c b a a))

; _____ E

(define (cumulative-sums item)
  (foldl (lambda (x cur)
           (append cur
                   (list (+ x (last cur)))))
         (list 0)
         item))

(cumulative-sums '(1 2 3 4 5))
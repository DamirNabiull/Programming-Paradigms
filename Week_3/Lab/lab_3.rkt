#lang slideshow

; HELPERS
; For 3.1
(define (inc-length x cur)
  (+ cur 1))

; For 3.4
(define (append_if_cond x lst)
  (cond
    [(not (ormap (lambda (y) (equal? y x)) lst)) (append lst (list x))]
    [else lst]))

; EX 3.1
(define (len-via-foldl lst)
  (foldl inc-length 0 lst))

(len-via-foldl '(1 2 3 4 5))

; EX 3.2
(define (len-via-map lst)
  (apply + (map (lambda (x) 1)
              lst)))

(len-via-map '(1 2 3 4 5))

; EX 3.3
(define (average lst)
  (/ (apply + lst) (len-via-foldl lst)))

(average '(1 2 3 4 5 6))

; EX 3.4
(define (my-remove-duplicates lst)
  (foldl append_if_cond empty lst))

(my-remove-duplicates `(1 1 2 3 3 3 4 5 6 6 6 6))

; EX 3.5
(define (my-remove-duplicates-with lst func)
  (foldl (lambda (x n_lst)
           (cond
             [(not (ormap (lambda (y) (func y x)) n_lst)) (append n_lst (list x))]
             [else n_lst]))
         empty lst))

(my-remove-duplicates-with `(1 1 2 3 3 3 4 5 6 6 6 6 1 2) equal?)
(my-remove-duplicates-with `(1 1 2 3 3 3 4 5 6 6 6 6 1 2) (lambda (t z) (= z (* t 3))))

; EX 3.6
; My predicat don't add value that 3 times bigger than any other value in list

; EX 3.7
(define (for-range a b func)
  (map func (range a b)))

(for-range 1 10 (lambda (x) (* x x)))

; EX 3.8
(define (for-range-2D a b func)
  (map (lambda (x)
         (map (lambda (y)
                (func x y))
              (range a b)))
       (range a b)))

(for-range-2D 1 4 (lambda (x y) (cons x y)))

; EX 3.9

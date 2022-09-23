#lang slideshow

; Lab 4

; Exercise 1
(define (x-abs-difference item-1 item-2)
  (abs (- (car item-1) (car item-2))))

(define (y-abs-difference item-1 item-2)
  (abs (- (cdr item-1) (cdr item-2))))

(define (on-diagonal? item-1 item-2)
  (equal? (x-abs-difference item-1
                            item-2)
          (y-abs-difference item-1
                            item-2)))

(define (on-line? item-1 item-2)
  (or (equal? (x-abs-difference item-1
                                item-2)
              0)
      (equal? (y-abs-difference item-1
                                item-2)
              0)))

(define (attacks? queen-1 queen-2)
  (or (on-diagonal? queen-1 queen-2)
      (on-line? queen-1 queen-2)))

(attacks? (cons 1 1) (cons 2 2))
(attacks? (cons 1 1) (cons 2 3))
(attacks? (cons 1 1) (cons 1 2))

; Exercise 2
(define (attacks-helper queen-1)
  (lambda (queen-2) (attacks? queen-1 queen-2)))

(define (attacks-any? queen-1 queens-list)
  (ormap (attacks-helper queen-1) queens-list))

(attacks-any? (cons 1 1) (list (cons 6 3) (cons 2 3) (cons 4 5)))
(attacks-any? (cons 1 1) (list (cons 1 3) (cons 2 3) (cons 4 5)))

; Exercise 3
(define (no-attacks-list queens-list)
  (for*/list ([i queens-list]
              [j queens-list]
              #:when (not (equal? i j)))
    (not (attacks? i j))))

(define (no-attacks? queens-list)
  (andmap (lambda (x) x)
          (no-attacks-list queens-list)))

(no-attacks? (list (cons 6 3) (cons 2 3) (cons 4 5)))
(no-attacks? (list (cons 6 3) (cons 2 4) (cons 4 7)))

; Exercise 4
(define (for-range a b func)
  (map func (inclusive-range a b)))

; Exercise 5
(define (generate-column index)
  (for-range 1 4 (lambda (x) (cons index x))))

(define (naive-four-queens)
  (for*/list ([a (generate-column 1)]
              [b (generate-column 2)]
              [c (generate-column 3)]
              [d (generate-column 4)]
              #:when (no-attacks? (list a b c d)))
    (list a b c d)))
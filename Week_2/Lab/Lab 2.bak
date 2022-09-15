#lang slideshow
; HELPERS
(define (bool-to-int val)
  (cond
    [val 1]
    [else 0])
  )

(define (int-to-bool val)
  (cond
    [(= val 0) #f]
    [else #t])
  )

; CONSTANTS
(define line-size 25)
(define splitter-size 1)

; TASK 1
(define (render-bit bit)
  (cond
    [(= bit 1)
     (filled-rectangle line-size line-size)]
    [else
     (rectangle line-size line-size)]))

(render-bit 0)
(render-bit 1)

; TASK 2
(define (render-bits bits)
  (cond
    [(empty? bits)(hc-append splitter-size)]
    [(cons? bits)(hc-append splitter-size (render-bit (first bits))
                            (render-bits (rest bits)))]
    )
  )

(render-bits '(1 0 1 1 1 0 1))

; TASK 3
(define (count-ones bits)
  (define (helper bits ans)
    (cond
      [(empty? bits) ans]
      [(cons? bits) (helper (rest bits) (+ ans (first bits)))]
      )
    )
  (helper bits 0)
  )

(count-ones '(1 0 1 1 1 0 1))

; TASK 4
(define (trailing-zeros bits)
  (define (helper bits val)
    (cond
      [(or (empty? bits) (= (first bits) 1)) val]
      [else (helper (rest bits) (+ val 1))]
     )
    )
  (helper (reverse bits) 0)
  )

(trailing-zeros '(1 0 1 1 1 0 0))

; TASK 5
(define (increment bits)
  (define (helper bits val new-bits)
    (cond
      [(and (empty? bits) (> val 0)) (reverse (append new-bits (list val)))]
      [(empty? bits) (reverse new-bits)]
      [else (helper (rest bits) (bool-to-int (and (int-to-bool val) (int-to-bool (first bits)))) (append new-bits (list (bool-to-int (xor (int-to-bool val) (int-to-bool (first bits)))))))]
      )
    )
  (helper (reverse bits) 1 empty)
  )

(increment '(1 0 1 1 1 0 1))
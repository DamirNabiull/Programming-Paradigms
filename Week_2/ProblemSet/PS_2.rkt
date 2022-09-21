#lang slideshow
; *********************************************************************************************************************************
; TASK 1 A
(writeln "**** TASK 1 A ****")

(define (binary-to-decimal bits)
  (define (helper bits val multiplier)
    (cond
      [(empty? bits) val]
      [(cons? bits) (helper (rest bits) (+ val (* multiplier (first bits))) (* multiplier 2))]
      )
    )
  (helper (reverse bits) 0 1)
  )

(binary-to-decimal '(1 0 1 1 0))

; TASK 1 B
(writeln "**** TASK 1 B ****")

(define (count-zeros bits)
  (define (helper bits val zeros)
    (cond
      [(empty? bits) val]
      [(= (first bits) 1) (helper (rest bits) (+ val zeros) 0)]
      [else (helper (rest bits) val (+ zeros 1))]
      )
    )
  (helper (reverse bits) 0 0)
  )

(count-zeros '(0 0 0 1 0 1 1 0))

; TASK 1 C
(writeln "**** TASK 1 C ****")

(define (encode-with-lengths bits)
  (define (helper bits val prev ans)
    (cond
      [(and (empty? bits) (= prev 1)) (reverse (append ans (list val)))]
      [(empty? bits) (reverse ans)]
      [(= (first bits) prev)(helper (rest bits) (+ val 1) prev ans)]
      [else (helper (rest bits) 1 (first bits) (append ans (list val)))]
      )
    )
  (helper (rest (reverse bits)) 1 (first (reverse bits)) empty)
  )

(encode-with-lengths '(0 0 0 1 1 0 1 1 1 0 0))

; TASK 1 D
(writeln "**** TASK 1 D ****")

(define (binary-odd? bits)
  (= (first (reverse bits)) 1)
  )

(binary-odd? '(1 0 1 1 0))
(binary-odd? '(1 0 1 1 1))

; TASK 1 E
(writeln "**** TASK 1 E ****")

(define (decrement bits)
  (define (helper val ans)
    (cond
      [(< val 0) (append ans (list 0))]
      [(= val 0) ans]
      [else (helper (quotient val 2)(append ans (list (remainder val 2))))]
      )
    )
  (helper (- (binary-to-decimal bits) 1) empty)
  )

(decrement '(1 0 1 1 0))
(decrement '(1 0 0 0 0))
(decrement '(0))
; *********************************************************************************************************************************
(writeln "*********************************************************************************************************************************")


; *********************************************************************************************************************************
; TASK 2 A
(writeln "**** TASK 2 A ****")

(define (alternating-sum bits)
  (define (helper bits multiplier)
    (cond
      [(empty? bits) 0]
      [else (+ (* (first bits) multiplier)
               (helper (rest bits) (* multiplier -1)))]
      )
    )
  (helper bits 1)
  )
(alternating-sum (list 6 2 4 1 3 9))

; TASK 2 B
(writeln "**** TASK 2 B ****")
(alternating-sum (list 1 2 3 4 5))

; (alternating-sum (list 1 2 3 4 5))
; (helper bits 1)
; (helper '(1 2 3 4 5) 1)
; (+ (* (first bits) multiplier)(helper (rest bits) (* multiplier -1)))
; (+ (* 1 1)(helper (rest '(1 2 3 4 5)) (* 1 -1)))
; (+ 1 (helper '(2 3 4 5) -1))
; (+ 1 (+ (* (first bits) multiplier)(helper (rest bits) (* multiplier -1))))
; (+ 1 (+ (* 2 -1)(helper (rest '(2 3 4 5)) (* -1 -1))))
; (+ 1 (+ -2 (helper '(3 4 5) 1)))
; (+ 1 (+ -2 (+ (* (first bits) multiplier)(helper (rest bits) (* multiplier -1)))))
; (+ 1 (+ -2 (+ (* 3 1)(helper (rest '(3 4 5)) (* 1 -1)))))
; (+ 1 (+ -2 (+ 3 (helper '(4 5) -1))))
; (+ 1 (+ -2 (+ 3 (+ (* (first bits) multiplier)(helper (rest bits) (* multiplier -1))))))
; (+ 1 (+ -2 (+ 3 (+ (* 4 -1)(helper (rest '(4 5)) (* -1 -1))))))
; (+ 1 (+ -2 (+ 3 (+ -4 (helper '(5) 1)))))
; (+ 1 (+ -2 (+ 3 (+ -4 (+ (* (first bits) multiplier)(helper (rest bits) (* multiplier -1)))))))
; (+ 1 (+ -2 (+ 3 (+ -4 (+ (* 5 1)(helper (rest '(5)) (* 1 -1)))))))
; (+ 1 (+ -2 (+ 3 (+ -4 (+ 5 (helper '() -1))))))
; (+ 1 (+ -2 (+ 3 (+ -4 (+ 5 0)))))
; (+ 1 (+ -2 (+ 3 (+ -4 5))))
; (+ 1 (+ -2 (+ 3 1)))
; (+ 1 (+ -2 4))
; (+ 1 2)
; 3


; TASK 2 C
(writeln "**** TASK 2 C ****")

(define (alternating-sum-tail bits)
  (define (helper bits multiplier prev)
    (cond
      [(empty? bits) prev]
      [else (helper (rest bits) (* multiplier -1) (+ prev (* (first bits) multiplier)))]
      )
    )
  (helper bits 1 0)
  )

(alternating-sum-tail (list 1 2 3 4 5))

; In the explicit recursion we firstly generate the complete equation from the start to the end. After generation we calculate the answer from the end of equation to the start.
; In the tail recursion we firstly compute local answer and then push it further in recursion. In this case we don't need to compute complete equation and evaluate the value from the end to the start.

; *********************************************************************************************************************************
(writeln "*********************************************************************************************************************************")


; *********************************************************************************************************************************
; TASK 3
(writeln "**** TASK 3 ****")
(define (dec n) (- n 1))

(define (f n)
  (cond
    [(<= n 2) (- 10 n)]
    [else (* (f (dec (dec n))) (f (dec n)))]))

(f 3)

; (f 3)
; (* (f (dec (dec n))) (f (dec n)))
; (* (f (dec (dec 3))) (f (dec 3)))
; (* (f (dec (- 3 1))) (f (- 3 1)))
; (* (f (dec 2)) (f 2))
; (* (f (- 2 1)) (f 2))
; (* (f 1) (f 2))
; (* (- 10 1) (- 10 2))
; (* 9 8)
; 72


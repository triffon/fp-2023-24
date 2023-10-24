#lang racket

; Зад.1
(define (sum-interval a b)
  (define (loop i res)
    (if (> i b) res
        (loop (+ i 1) (+ res i))))
  (loop a 0))

; Зад.2
(define (count-digit d n)
  (define (loop res n)
    (cond [(= n 0) res]
          [(= d (remainder n 10)) (loop (+ res 1)
                                        (quotient n 10))]
          [else (loop res (quotient n 10))]))
  (loop 0 n))

; Зад.3
(define (reverse-digits n)
  (define (loop n res)
    (if (zero? n) res
        (loop (quotient n 10) (+ (* res 10) (remainder n 10)))))
  (loop n 0))

; Зад.4
(define (divisors-sum n)
  (define (loop i res)
    (cond [(> i n) res]
          [(zero? (remainder n i)) (loop (+ i 1) (+ res i))]
          [else (loop (+ i 1) res)]))
  (loop 1 0))

; Зад.5
(define (perfect? n)
  (= (divisors-sum n) (* 2 n)))

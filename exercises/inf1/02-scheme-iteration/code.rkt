#lang r5rs

(define (count-digits n)
  (if (<= n 9)
      1
      (+ 1 (count-digits (quotient n 10)))))

(define (count-digits-iter n)
  (count-digits-iter-loop n 1))

(define (count-digits-iter-loop n count)
  (if (<= n 9)
      count
      (count-digits-iter-loop (quotient n 10) (+ 1 count))))

(define (fib n)
  (if (<= n 1)
      1
      (+ (fib (- n 1)) (fib (- n 2)))))

(define (fib-iter n)
  (define (loop i a b)
    (if (= i n)
        b
        (loop (+ i 1) b (+ a b))))

  (loop 0 1 1))

(define (fact-iter n)
  (define (loop i accum)
    (if (> i n)
        accum
        (loop (+ 1 i) (* i accum))))

  (loop 1 1))

(define (divides? a b)
  (= 0 (remainder a b)))

(define (count-divisors n)
  (define (loop i count)
    (if (< n i)
        count
        (if (divides? n i)
            (loop (+ 1 i) (+ 1 count))
            (loop (+ 1 i) count))))

  (loop 1 0))

(define (prime? n)
  (= (count-divisors n) 2))
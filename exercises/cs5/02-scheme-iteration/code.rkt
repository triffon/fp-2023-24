#lang r5rs

(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(define (fact-helper2 start n product)
  (if (= start n)
      (* product start)
      (fact-helper2 (+ start 1) n (* product start))))



(define (fact-iter n)
  (define (loop n product)
    (if (= n 0)
      product
      (loop (- n 1) (* product n))))
  
  (loop n 1))

(define (fib n)
  (if (<= n 1)
      1
      (+ (fib (- n 1)) (fib (- n 2)))))

(define (fib-helper n pprev prev)
  (if (= n 0)
      prev
      (fib-helper (- n 1) prev (+ prev pprev))))

(define (fib-iter n)
  (fib-helper n 0 1))

(define (count-digits n)
  (define (loop n count)
    (if (<= n 9)
        count
        (loop (quotient n 10) (+ 1 count))))

  (loop n 1))

(define (divides? a b)
  (= 0 (remainder a b)))

(define (count-divisors n)
  (define (loop i count)
    (if (= i 0)
        count
        (loop
         (- i 1)
         (if (divides? n i)
             (+ 1 count)
             count))))
  (loop n 0))

(define (prime? n)
  (= (count-divisors n) 2))
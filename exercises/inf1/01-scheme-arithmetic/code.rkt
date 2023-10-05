#lang r5rs

(define (add1 x)
  (+ 1 x))

(define (fact x)
  (if (= x 0)
      1
      (* x (fact (- x 1)))))

(define (count-digits n)
  (if (< n 0)
      (count-digits (- 0 n))
      (if (<= n 9)
          1
          (+ 1 (count-digits (quotient n 10))))))

(define (fib n)
  (if (= n 0)
      0
      (if (= n 1)
          1
          (+ (fib (- n 1)) (fib (- n 2))))))

(define (fib2 n)
  (if (or (= n 0) (= n 1))
      n
      (+ (fib2 (- n 1)) (fib2 (- n 2)))))

(define (fib3 n)
  (cond
    ((= n 0) 0)
    ((= n 1) 1)
    (else (+ (fib3 (- n 1)) (fib3 (- n 2))))))

(define (succ n)
  (+ 1 n))

(define (pred n)
  (define (look-for-pred-starting-from z)
    (if (= (succ z) n)
        z
        (look-for-pred-starting-from (succ z))))
  
  (look-for-pred-starting-from 0))

(define (add a b)
  (if (= b 0)
      a
      (add (succ a) (pred b))))

(define (multiply a b)
  (if (= b 0)
      0
      (add a (multiply a (pred b)))))
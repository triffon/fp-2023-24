#lang r5rs

(define (add5 x)
  (+ x 5))

; (fact x) -- смята x!
(define (fact x)
  (if (< x 1)
      1
      (* x (fact (- x 1)))))

; (fib n) -- намира n-тото число на Фибоначи
(define (fib n)
  (if (<= n 1)
      1
      (+ (fib (- n 1)) (fib (- n 2)))))

(define (succ n)
  (+ 1 n))

(define (pred n)
  (define (look-for-pred x)
    (if (= (succ x) n)
      x
      (look-for-pred (succ x))))

  (look-for-pred 0))

(define (add a b)
  (if (= b 0)
      a
      (add (succ a) (pred b))))
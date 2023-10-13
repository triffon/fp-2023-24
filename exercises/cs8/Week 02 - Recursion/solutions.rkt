#lang racket

(define (sum-n-rec n)
  (if (zero? n) 0
      (+ n (sum-n-rec (- n 1)))))

(define (sum-n n)
  (define (for i res)
    (if (zero? i) res
        (for (- i 1) (+ res i))))
  (for n 0))

(define (fibonacci-rec n)
  (if (<= n 2) 1
      (+ (fibonacci-rec (- n 1))
         (fibonacci-rec (- n 2)))))

(define (fib n)
  (define (for f-1 f-2 i)
    (if (<= i n)
        (for (+ f-1 f-2) f-1 (+ i 1))
        f-1))
  (for 1 1 3))

(define (sum n)
  (let* ([a 5]
         [b (+ a 1)])
    (+ a b)))

(define (fast-pow x n)
  (define (while x n res)
    (cond [(zero? n) res]
          [(even? n) (while (* x x) (/ n 2) res)]
          [else (while x (- n 1) (* res x))]))
  (while x n 1))

(define (prime? n)
  (define (for i)
    (cond [(> (* i i) n) #t]
          [(zero? (remainder n i)) #f]
          [else (for (+ i 1))]))
  (if (= n 1) #f
      (for 2)))

(define (square? n)
  (define (bin-search left right)
    (let ([middle (quotient (+ left right) 2)])
      (cond [(= (* middle middle) n) #t]
            [(> left right) #f]
            [(> (* middle middle) n)
             (bin-search left (- middle 1))]
            [else (bin-search ( + middle 1) right)])))
  (bin-search 1 n))










 






 
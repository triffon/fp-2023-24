#lang racket

(define-syntax delay
  (syntax-rules () ((delay x)
                    (lambda () x))))

(define-syntax cons-stream
  (syntax-rules () ((cons-stream h t)
                    (cons h (delay t)))))

(define head car)
(define (tail s) ((force (cdr s))))

(define (take n s)
  (if (or (null? s)
          (zero? n))
      '()
      (cons (head s)
            (take (- n 1)
                  (tail s)))))

(define (nats-from n)
  (cons-stream n (nats-from (+ n 1))))

(define nats
  (nats-from 0))

(define (map-stream f s)
  (if (null? s) '()
      (cons-stream (f (head s))
                   (map-stream f (tail s)))))

(define (fib-helper fib1 fib2)
  (let ([sum (+ fib1 fib2)])
    (cons-stream sum
               (fib-helper fib2 sum))))

(define fib
  (cons-stream 0
               (cons-stream 1
                            (fib-helper 0 1))))

(define (zip-with f lst1 lst2)
  (if (or (null? lst1)
          (null? lst2))
      '()
      (cons (f (car lst1)
               (car lst2))
            (zip-with f
                      (cdr lst1)
                      (cdr lst2)))))

(define pascal-triangle
  (cons-stream '(1)
               (map-stream
                (lambda (x)
                  (zip-with +
                            (cons 0 x)
                            (append x
                                    (list 0))))
                pascal-triangle)))













                  
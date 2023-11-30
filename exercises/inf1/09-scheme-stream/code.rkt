#lang racket

(define (trace msg x)
  (display (list 'trace msg x))
  x)

(define (even? x)
  (= 0 (remainder (trace 'even? x) 2)))

(define-syntax my-delay
  (syntax-rules ()
    ((my-delay x) (lambda () x))))

(define (my-force f-x)
  (f-x))

(define-syntax my-and
  (syntax-rules ()
    ((my-and x y) (if x (if y #t #f) #f))))

(define-syntax stream-cons
  (syntax-rules ()
    ((stream-cons elem stream) (cons elem (my-delay stream)))))

(define (gen-nats-from n)
  (stream-cons n (gen-nats-from (+ 1 n))))

(define (stream-first str)
  (car str))

(define (stream-rest str)
  (my-force (cdr str)))

(define nats (gen-nats-from 0))
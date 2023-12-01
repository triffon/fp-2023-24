#lang racket

(define-syntax my-delay
  (syntax-rules ()
    ((my-delay expr) (lambda () expr))))

(define (my-force f0)
  (f0))

(define-syntax my-stream-cons
  (syntax-rules ()
    ((my-stream-cons fst ps)
     (cons fst (my-delay ps)))))

(define (my-stream-first ps)
  (car ps))

(define (my-stream-rest ps)
  (my-force (cdr ps)))


(define (generate-nats-from n)
  (my-stream-cons n (generate-nats-from (+ 1 n))))

(define nats (generate-nats-from 0))

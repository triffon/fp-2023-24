#lang racket

(delay (* 2 3))
(force (delay (* 2 3)))

(define empty-stream '())

;(define (cons-stream h t)
;  (cons h (delay t))
;  )

(define-syntax cons-stream
  (syntax-rules () ((cons-stream h t) (cons h (delay t))))
  )

(define s (cons-stream 1 (cons-stream 2 (cons-stream 3 empty-stream))))
s

(define (repeat v)
  (cons-stream v (repeat v))
  )

(repeat 5)

(define head car)
(define (tail s)
  (force (cdr s)))

(head (repeat 5))

(define (stream-take n stream)
  (if (or (zero? n) (empty? stream)) '() (cons (head stream) (stream-take (sub1 n) (tail stream))))
  )

(stream-take 5 (repeat 5))
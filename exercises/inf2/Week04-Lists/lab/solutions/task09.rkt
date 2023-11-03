#lang racket

(define (insert-at x position xs)
  (cond
    [(zero? position) (cons x xs)]
    [(empty? xs) (list x)]
    [else (cons (car xs) (insert-at x (sub1 position) (cdr xs)))]
    )
  )

(equal? (insert-at 1 0 '()) '(1))
(equal? (insert-at 1 0 '(2)) '(1 2))
(equal? (insert-at 10 1 '(1 2 3)) '(1 10 2 3))
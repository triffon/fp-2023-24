#lang racket

(define (num-to-xs n)
  (define (helper current result)
    (if (zero? current)
        result
        (helper (quotient current 10) (cons (remainder current 10) result))
        )
    )
  (if (zero? n) '(0) (helper n '()))
  )

(define (count-occurrences n xs)
  (length (filter (λ (x) (= x n)) xs))
  )

(define (get-distribution n)
  (sort (remove-duplicates
         (map (λ (digit) (cons digit (count-occurrences digit (num-to-xs (* n n))))) (num-to-xs (* n n))))
        (λ (x y) (< (car x) (car y))))
  )

(equal? (get-distribution 0) '((0 . 1)))
(equal? (get-distribution 2) '((4 . 1)))
(equal? (get-distribution 8) '((4 . 1) (6 . 1)))
(equal? (get-distribution 123) '((1 . 2) (2 . 1) (5 . 1) (9 . 1)))
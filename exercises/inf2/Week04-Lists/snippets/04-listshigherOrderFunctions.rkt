#lang racket

(sort '(1 3 2) >)

(apply + '(1 2 3 4 5))

(map (λ (x) (* x 2)) '(1 2 3 4 5))
(map add1 '(1 2 3 4 5))

(filter even? '(1 2 3 4 5))
(filter (λ (x) (zero? (remainder x 3))) '(0 1 2 3 4 5 6))

(foldl (λ (x y) (* x y)) 1 '(1 2 3 4 5))
(foldr (λ (x y) (and x y)) #f '(#t #t #t #t #t))
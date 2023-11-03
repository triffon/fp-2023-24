#lang racket

(ormap even? '(1 2 3 4 5))

(andmap even? '(1 2 3 4 5))

(take '(1 2 3 4) 0)
(take '(1 2 3 4) 2)

(takef '(2 4 5 8) even?)
(takef '(2 4 6 8) odd?)

(drop '(1 2 3 4) 0)
(drop '(1 2 3 4) 2)

(dropf '(2 2 5 8 56) even?)
(dropf '(2 2 4 8 56) even?)
#lang racket

(display "Enter a number:")
(define input (read-line))

(displayln "You entered:")
(displayln input)

(number? input)
(+ 3 (string->number input))
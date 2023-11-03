#lang racket

; (cons a b) -> (a . b)
; (car p)    -> p:(a . b) -> a
; (cdr p)    -> p:(a . b) -> b

(define (len l)
  (if (null? l)
      0
      (+ 1 (len (cdr l)))))

(define (exists? l p)
  (if (null? l)
      #f
      (or (p (car l)) (exists? (cdr l) p))))

(define (at n l)
  (if (= n 0)
      (car l)
      (at (- n 1) (cdr l))))

(define (push x l)
  (if (null? l)
      (list x)
      (cons (car l) (push x (cdr l)))))
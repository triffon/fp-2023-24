#lang racket

(define (at n l)
  (if (= n 0)
      (car l)
      (at (- n 1) (cdr l))))

(define (mat-at m i j)
  (at j (at i m)))

(define (mat-map f m)
  (map (lambda (row) (map f row)) m))

(define (scalmul x m)
  (mat-map (lambda (el) (* x el)) m))

(define (transpose m)
  (if (or (null? m) (null? (car m)))
      (list)
      (cons (map car m) (transpose (map cdr m)))))

(define (zip op l1 l2)
  (if (or (null? l1) (null? l2))
      (list)
      (cons (op (car l1) (car l2)) (zip op (cdr l1) (cdr l2)))))

(define (vect-mul v1 v2)
  (foldr 0 + (zip * v1 v2)))

(define (mat-multiply m1 m2)
  (define (multiply-row row)
    (map (lambda (column) (vect-mul row column)) (transpose m2)))
  (map multiply-row m1))
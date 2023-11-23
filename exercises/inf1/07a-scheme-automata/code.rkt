#lang racket

(define assoc-empty (list))

(define (assoc-get l k)
  (if (null? l)
      #f
      (if (equal? (car (car l)) k)
          (cdr (car l))
          (assoc-get (cdr l) k))))

(define (assoc-set l k v)
  (if (null? l)
      (list (cons k v))
      (if (equal? (car (car l)) k)
          (cons (cons k v) (cdr l))
          (cons (car l) (assoc-set (cdr l) k v)))))

(define (assoc-map f l)
  (map (lambda (pair) (cons (car pair) (f (cdr pair)))) l))

(define (assoc-filter p? l)
  (filter (lambda (pair) (p? (cdr pair))) l))

(define (assoc-set-pair pair l)
  (assoc-set l (car pair) (cdr pair)))

(define (assoc-merge l1 l2)
  (foldr assoc-set-pair l1 l2))
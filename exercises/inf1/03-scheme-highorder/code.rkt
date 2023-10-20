#lang r5rs

(define (infix a f b)
  (f a b))

(define (make-adder2 a)
  (define (f x)
    (+ a x))

  f)

(define (make-adder a)
  (lambda (x) (+ a x)))

(define (succ x)
  (+ 1 x))

(define (push-five x)
  (+ 5 (* 10 x)))

(define (push-eight x)
  (+ 8 (* 10 x)))

(define (o f g)
  (lambda (x) (f (g x))))

(define (repeated f n x)
  ; (f (f (f ... (f x) ...)))
  (if (= n 0)
      x
      (f (repeated f (- n 1) x))))

; (define (repeat f n)
;   (lambda (x) (repeated f n x)))

(define (id x) x)
; (define id (lambda (x) x))

; (define (repeat f n)
;   (if (= n 0)
;       id
;       (lambda (x) (f ((repeat f (- n 1)) x)))))

; (define (repeat f n)
;   (if (= n 0)
;       id
;       (o f (repeat f (- n 1)))))

(define (repeat f n)
  (repeated (lambda (g) (o f g)) n id))

(define (accumulate op init f begin end)
  (if (> begin end)
      init
      (op (f begin) (accumulate op init f (+ begin 1) end))))

(define (even? n)
  (= (remainder n 2) 0))

(define (count p begin end)
  (accumulate + 0 (lambda (x) (if (p x) 1 0)) begin end))

(define (or2 x y)
  (or x y))

(define (and2 x y)
  (and x y))

(define (exists? p a b)
  (accumulate or2 #f p a b))

(define (forall? p a b)
  (accumulate and2 #t p a b))

(define (repeat3 f n)
  (accumulate o id (lambda (x) f) 1 n))
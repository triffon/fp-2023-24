#lang r5rs

(define (infix a op b)
  (op a b))

(define (make-adder a)
  (lambda (x) (+ a x)))

(define (square x)
  (* x x))

(define (o f g)
  (lambda (x) (f (g x))))

(define (id x) x)

(define (repeated n f x)
  ((repeat n f) x))

; (define (repeat n f)
;   (lambda (x) (repeated n f x)))

; (define (repeated n f x)
;   (if (= n 0)
;       x
;       (f (repeated (- n 1) f x))))

(define (repeat n f)
  (if (= n 0)
      id
      (o f (repeat (- n 1) f))))

(define (accumulate op init f begin end)
  (if (> begin end)
      init
      (op (f begin) (accumulate op init f (+ begin 1) end))))

(define (count-rec p a b)
  (cond
    ((> a b) 0)
    ((p a) (+ 1 (count-rec p (+ 1 a) b)))
    (else (count-rec p (+ 1 a) b))))

(define (let-demo foo)
  (letrec
      ((fact (lambda (x) (if (= x 0) 1 (* x (fact (- x 1))))))
       (even? (lambda (x) (if (= x 0) #t (odd? (- x 1)))))
       (odd? (lambda (x) (if (= x 0) #f (even? (- x 1))))))
    (even? foo)))
    


(define (count p a b)
  (accumulate + 0 (lambda (x) (if (p x) 1 0)) a b))
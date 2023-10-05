#lang racket

;; 1.
(define (add a b)
  (+ a b))

;; or even (but not quite the same... why?):
;(define add +)

;; 2.
(define (is-even? number)
  (= 0 (remainder number 2)))

;; 3.
(define (signum number)
  (cond
    ((= 0 number) 0)
    ((< number 0) -1)
    (else 1)))

;; 4.
(define (root? x)
  (= 0
     (+
      (* 3 x x)
      (* 2 x)
      -1)))

;; 5.
(define (triangle? a b c)
  (and
   (> (+ a b) c)
   (> (+ a c) b)
   (> (+ b c) a)))

;; 6.
; indexing from 1
(define (fibonacci n)
  (cond
    ((= 1 n) 0)
    ((= 2 n) 1)
    (else (+
           (fibonacci (- n 1))
           (fibonacci (- n 2))))))

;; 7.
(define (sum-interval a b)
  (if (> a b)
      0
      (+ a (sum-interval (+ 1 a) b))))

;; 8.
(define (power base exponent)
  (cond
    ((< exponent 0)
     (/ 1 (power base (- exponent))))
    ((= exponent 0)
     1)
    (else
     (* base (power base (- exponent 1))))))

;; 9.
(define (count-digits number)
  (if (< -10 number 10)
      1
      (+ 1 (count-digits (quotient number 10)))))

;; 10.
(define (prepend-digit number digit)
  (+ number
     (* digit
        (power 10 (count-digits number)))))

(define (reverse-digits number)
  (if (< -10 number 10)
      number
      (prepend-digit
        (reverse-digits (quotient number 10))
        (remainder number 10))))

;; or maybe... like that?
(define (Y f)
  ((lambda (x) (f (lambda (y) ((x x) y))))
   (lambda (x) (f (lambda (y) ((x x) y))))))

(define reverse-digits-recursion???
  (Y (lambda (pr)
       (lambda (n)
         (if (< -10 n 10)
             n
             ((lambda (rest)
                (+ rest (* (remainder n 10) (expt 10 (count-digits rest)))))
              (pr (quotient n 10))))))))

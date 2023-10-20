#lang racket

(define f
  (lambda (a b)
    (+ a b)))

;(define (f a b)
;  (+ a b))

(define (f1 a b)
  (let ((g (λ (x)
             (+ x 1))))
    (+ a (g b))))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (derive f)
  (let ([h 0.000001])
    (lambda (a)
      (/ (- (f (+ a h))
            (f a))
         h))))

(define id
  (lambda (x) x))

(define (n+ n)
  (lambda (x)
    (+ x n)))

(define 1+ (n+ 1))
(define 2+ (n+ 2))

(define (repeat f n)
  (define (while i res)
    (if (zero? i) res
        (while (- i 1) (compose f res))))
  (while n id))

(define (accumulate operation null-value begin end term next)
  (if (> begin end) null-value
      (operation (term begin)
                 (accumulate operation null-value (next begin) end term next))))

(define (accumulate-iter operation null-value begin end term next)
  (if (> begin end) null-value
      (accumulate-iter operation
                       (operation null-value
                                  (term begin))
                       (next begin)
                       end term next)))

(define (sum-odd-squares a b)
  (accumulate-iter + 0 a b (lambda (x)
                             (if (even? x) 0
                                 (* x x))) 1+))

(define (argmax f a b)
  (accumulate-iter (lambda (res x)
                     (if (> (f x)
                            (f res))
                         x res))
                     a (+ a 1) b id 1+))

(define (all? pred? a b)
  (accumulate-iter (lambda (res x)
                     (and res x)) #t a b pred? 1+))

(define (any? pred? a b)
  (accumulate-iter (lambda (res x)
                     (or res x)) #f a b pred? 1+))

(define (count-digits n)
  (accumulate-iter + 0 1 n (lambda (x) 1)
                   (lambda (x)
                     (* x 10))))

(define (count-pairs a b n)
  (accumulate-iter + 0 a b (lambda (x)
     (if (any? (lambda (y)
                 (= (+ y x) n)) x b) 1 0)) 1+))









   
#lang racket

(define (at lst n)
  (cond [(null? lst) 'undefined]
        [(zero? n) (car lst)]
        [else (at (cdr lst) (- n 1))]))

(define (natural n)
  (define (for i res)
    (if (zero? i) res
        (for (- i 1) (cons i res))))
  (for n '()))

(define (take-n n lst)
  (if (or (zero? n)
          (null? lst))
      '()
      (cons (car lst) (take-n (- n 1) (cdr lst)))))
          
(define (drop-n n lst)
  (if (or (null? lst)
          (zero? n))
      lst
      (drop-n (- n 1) (cdr lst))))

(define (zip lst1 lst2)
  (if (or (null? lst1)
          (null? lst2))
      '()
      (cons (cons (car lst1)
                  (car lst2))
            (zip (cdr lst1)
                 (cdr lst2)))))

(define (max-repeated lst)
  (define (count-equal-elements el l res)
    (if (or (null? l)
            (not (equal? el (car l))))
        res
        (count-equal-elements el (cdr l) (+ res 1))))
  (if (null? lst) 0
      (let ([consecutive-equal (count-equal-elements
                                (car lst)
                                (cdr lst)
                                1)])
        (max consecutive-equal
             (max-repeated (drop-n consecutive-equal
                                   lst))))))

















 
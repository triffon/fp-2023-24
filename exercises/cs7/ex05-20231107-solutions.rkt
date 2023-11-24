#lang racket
(define head car)
(define tail cdr)

(define (length* lst)
  (if (null? lst)
      0
      (+ 1 (length* (tail lst)))))
(define (length** lst)
  (define (loop lst res)
    (if (null? lst)
        res
        (loop (tail lst) (+ res 1))))
  (loop lst 0))

(define (reverse* lst)
  (define (loop lst res)
    (if (null? lst)
        res
        (loop (tail lst)
              (cons (head lst) res))))
  (loop lst '()))

; Също вградена
(define (foldr* op nv lst)
  (if (null? lst) nv
      (op (head lst)
          (foldr* op nv (tail lst)))))

(define (length*** lst)
  (foldr (lambda (el res) (+ 1 res))
         0
         lst))

; Зад.1
(define (take n lst)
  (cond [(null? lst) '()]
        [(= n 0) '()]
        [else (cons (head lst)
                    (take (- n 1) (tail lst)))]))

(define (drop n lst)
  (cond [(null? lst) '()]
        [(= n 0) lst]
        [else (drop (- n 1) (tail lst))]))

; Зад.2
(define (all? p? lst)
  (or (null? lst)
      (and (p? (head lst))
           (all? p? (tail lst)))))
; можем да преизползваме all?
(define (any? p? lst)
  (and (not (null? lst))
       (or (p? (head lst))
           (any? p? (tail lst)))))

; Зад.3
(define (zip lst1 lst2)
  (zipWith cons lst1 lst2)) ;)

; Зад.4
(define (zipWith f lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      '()
      (cons (f (head lst1) (head lst2))
            (zipWith f (tail lst1) (tail lst2)))))

; Зад.5
(define (sorted? lst)
  (or (null? lst)
      (null? (tail lst))
      (and (<= (head lst) (head (tail lst)))
           (sorted? (tail lst)))))

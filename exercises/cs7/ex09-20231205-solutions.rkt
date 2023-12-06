#lang racket
(define empty-tree '())
(define (make-tree root left right) (list root left right))      ; не искаме просто (define make-tree list) - защо?
(define (make-leaf root) (make-tree root empty-tree empty-tree)) ; за удобство
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)

(define test-tree
  (make-tree 1
             (make-tree 2
                        (make-leaf 4)
                        (make-leaf 5))
             (make-tree 3
                        (make-tree 6
                                   (make-leaf 12)
                                   (make-leaf 13))
                        empty-tree)))

; Зад.10*
(define (valid-bst? t)
  (define (helper t from to)
    (define (inside? x)
      (and (or (not from) (>= x from))
           (or (not to) (<= x to))))
    (or (empty-tree? t)
        (and (inside? (root-tree t))
             (helper (left-tree t) from (root-tree t))
             (helper (right-tree t) (root-tree t) to))))
  (helper t #f #f))

(define not-a-bst
  (make-tree 2
           (make-tree 1
                   empty-tree
                   (make-leaf 3))
           empty-tree))

; Зад.14*
(define (same-as-code t)
  ; Инварианта: code е числото, чието двоично представяне
  ; е пътят от корена до текущия възел
  (define (helper t code)
    (if (empty-tree? t) '()
        (let [(lres (helper (left-tree t) (* 2 code)))
              (rres (helper (right-tree t) (+ (* 2 code) 1)))]
          (if (= (root-tree t) code)
              (append (list (root-tree t))
                      lres rres)
              (append lres rres)))))
  (helper t 1))

; Задачи от Второ контролно 2020/2021
(define (smallest-rect pts)
  (let [(xmin (apply min (map car pts)))
        (xmax (apply max (map car pts)))
        (ymin (apply min (map cdr pts)))
        (ymax (apply max (map cdr pts)))]
    (cons (cons xmin ymax)
          (cons xmax ymin))))

(define (inside? pt rect)
  (let [(xmin (car (car rect)))
        (xmax (car (cdr rect)))
        (ymin (cdr (cdr rect)))
        (ymax (cdr (car rect)))
        (x (car pt))
        (y (cdr pt))]
    (and (<= xmin x) (<= x xmax)
         (<= ymin y) (<= y ymax))))

(define head car)
(define tail cdr)
; От предишни упражнения
(define (zip lst1 lst2)
  (if (or (null? lst1) (null? lst2)) '()
      (cons (cons (head lst1) (head lst2))
            (zip (tail lst1) (tail lst2)))))

(define (cost L r k)
  (define (sq x) (* x x))
  (define (dist p1 p2)
    (* k (sqrt (+ (sq (- (car p1) (car p2)))
                  (sq (- (cdr p1) (cdr p2)))))))
  (let [(L* (filter (lambda (pt) (inside? pt r)) L))]
    (apply + (map (lambda (p) (dist (car p) (cdr p)))
                  (zip L* (tail L*))))))

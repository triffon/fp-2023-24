#lang racket

(define dl '((1 2) ((3 (4) 5) (((6)))) (7)))

(define (atom? l)
  (and (not (null? l)) (not (pair? l))))

(define (make-tree root left right) (list root left right))
(define (make-leaf root) (list root '() '()))
(define root car)
(define left cadr)
(define right caddr)
(define empty? null?)

(define t (make-tree 5
                        (make-tree 1
                                   (make-tree 4
                                              '()
                                              (make-leaf 13))
                                   (make-leaf 3))
                        (make-tree 8
                                   (make-tree 0
                                              (make-leaf 10)
                                              (make-leaf 9))
                                   (make-leaf 11))))

(define bst
  '(3 (1 () (2 () ()))
      (4 () (5 () ()))))

(define (deep-foldr op term nv lst)
  (cond [(null? lst) nv]
        [(atom? lst) (term lst)]
        [else (op (deep-foldr op term nv (car lst))
                  (deep-foldr op term nv (cdr lst)))]))

(define (deep-map f lst)
  (deep-foldr cons f '() lst))

(define (deep-member? el lst)
  (deep-foldr (lambda (x y) (or x y))
              (lambda (x) (equal? x el))
              #f
              lst))

(define (flatten lst)
  (deep-foldr append list '() lst))

(define (height t)
  (if (empty? t) 0
      (+ 1 (max (height (left t))
                (height (right t))))))

(define (map-tree f t)
  (if (empty? t) '()
      (make-tree (f (root t))
                 (map-tree f (left t))
                 (map-tree f (right t)))))

(define (level n t)
  (if (zero? n)
      (list (root t))
      (append (level (- n 1)
                     (left t))
              (level (- n 1)
                     (right t)))))

(define (fcons a b)
  (if (and a b)
      (cons a b)
      #f))

(define (path-to el t)
  (cond [(empty? t) #f]
        [(equal? el (root t))
         (list el)]
        [else (fcons (root t)
                     (or (path-to el (left t))
                         (path-to el (right t))))]))

(define (bst-insert el t)
  (cond [(empty? t)
         (make-leaf el)]
        [(> el (root t))
         (make-tree (root t)
                    (left t)
                    (bst-insert el (right t)))]
        [else (make-tree (root t)
                         (bst-insert el (left t))
                         (right t))]))
         







  
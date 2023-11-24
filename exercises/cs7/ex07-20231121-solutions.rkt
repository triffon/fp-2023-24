#lang racket
(define head car)
(define tail cdr)

; Зад.1
(define (dependent? lst1 lst2)
  (let [(coeff (/ (head lst2) (head lst1)))]
    (equal? (map (lambda (x) (* x coeff)) lst1)
            lst2)))

(dependent? '(2 6 4) '(3 9 6))
(dependent? '(1 2) '(3 4))

; Зад.1*
; "Разширяваме" вградената функция member, както и всяка друга
; която я извиква директно или индиректно до uniques.
(define (member-by cmp el lst)
  (cond [(null? lst) #f]
        [(cmp el (head lst)) #t]
        [else (member-by cmp el (tail lst))]))

(define (cons-if-not-by cmp x lst)
  (if (member-by cmp x lst) lst (cons x lst)))

(define (uniques-by cmp lst)
  (foldr (lambda (el res)
           (cons-if-not-by cmp el res))
         '()
         lst))

; Идеята е същата като на uniques: от групите
; линейно-зависими редове да оставим само по един
(define (pseudorank m)
  (length (uniques-by dependent? m)))

(pseudorank '((2 6 4)
              (6 3 6)
              (3 9 6)
              (8 4 8)
              (1 3 2)))

; Зад.2.
(define (gauss row1 row2)
  (let [(coeff (- (/ (head row2) (head row1))))]
    (map (lambda (x y) (+ (* coeff x) y))
              row1
              row2)))

; от минали упражнения
(define (all? p? lst)
  (or (null? lst)
      (and (p? (head lst))
           (all? p? (tail lst)))))
(define (dependent?* lst1 lst2)
  (all? zero? (gauss lst1 lst2))) ;)

(define (determinant m)
  (* (head (head m))
     (if (null? (tail m)) 1
         (determinant (map (lambda (row)
                             (tail (gauss (head m) row)))
                           (tail m))))))

; Дървета
(define (tree? t)
  (or (null? t)
      (and (list? t)
           (= (length t) 3))
           (tree? (cadr t))
           (tree? (caddr t))))

(define empty-tree '())
(define (make-tree root left right) (list root left right))      ; не искаме просто (define make-tree list) - защо?
(define (make-leaf root) (make-tree root empty-tree empty-tree)) ; за удобство
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)

(define test-tree
  (make-tree 10
             (make-tree 7
                        (make-leaf 10)
                        (make-leaf 2))
             (make-tree 3
                        (make-tree 4
                                   (make-leaf 1)
                                   (make-leaf 2))
                        empty-tree)))

; Зад.1.
(define (height t)
  (if (empty-tree? t) 0
      (+ 1 (max (height (left-tree t))
                (height (right-tree t))))))

; Зад.2.
(define (get-level n t)
  (cond [(empty-tree? t) '()]
        [(= n 0) (list (root-tree t))]
        [else (append (get-level (- n 1) (left-tree t))
                      (get-level (- n 1) (right-tree t)))]))

(get-level 2 test-tree)

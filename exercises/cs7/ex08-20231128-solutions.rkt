#lang racket
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

; Зад.5
(define (find-longest-path t)
  ; Вариант 1: "Кешираме" двата пътя като аргументи на помощна функция
  ;(define (longer lst1 lst2)
  ;  (if (> (length lst1) (length lst2)) lst1 lst2))
  ;(if (empty-tree? t) '()
  ;    (cons (root-tree t)
  ;          (longer (find-longest-path (left-tree t))
  ;                  (find-longest-path (right-tree t))))))
; Вариант 2 (лош): всеки път преизчислява пътищата (
;  (cond [(empty-tree? t) '()]
;        [(> (length (find-longest-path (left-tree t)))
;            (length (find-longest-path (right-tree t))))
;         (cons (root-tree t)
;               (find-longest-path (left-tree t)))]
;        [else
;         (cons (root-tree t)
;               (find-longest-path (right-tree t)))]))
; Вариант 3, проблем: променливите в let се оценяват дори да не се използват
; и преди тялото на let-а => ще се достъпят поддърветата преди проверката за празно
;  (let [(left-path (find-longest-path (left-tree t)))
;        (right-path (find-longest-path (right-tree t)))]
;    (cond [(empty-tree? t) '()]
;          [(> (length left-path) (length right-path))
;           (cons (root-tree t) left-path)]
;          [else (cons (root-tree t) right-path)])))
; Вариант 4: кешираме веднага след проверката
  (if (empty-tree? t) '()
      (let [(left-path (find-longest-path (left-tree t)))
            (right-path (find-longest-path (right-tree t)))]
        (if (> (length left-path) (length right-path))
            (cons (root-tree t) left-path)
            (cons (root-tree t) right-path)))))


(find-longest-path test-tree)

; Зад.6
(define (tree-map f t)
  (if (empty-tree? t) empty-tree
      (make-tree (f (root-tree t))
                 (tree-map f (left-tree t))
                 (tree-map f (right-tree t)))))

; Зад.7
; По дадено дърво връща списък то стойностите в него,
; получени при обхождане ляво-корен-дясно
(define (tree->list t)
  (if (empty-tree? t) '()
      (append (tree->list (left-tree t))
              (list (root-tree t))
              (tree->list (right-tree t)))))

; Зад.8
(define (bst-insert val t)
  (cond [(empty-tree? t) (make-leaf val)]
        [(< val (root-tree t)) (make-tree
                                (root-tree t)
                                (bst-insert val (left-tree t))
                                (right-tree t))]
        [else (make-tree (root-tree t)
                         (left-tree t)
                         (bst-insert val (right-tree t)))]))

; Зад.9
(define (tree-sort lst)
  (tree->list (foldl bst-insert empty-tree lst)))

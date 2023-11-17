# Седмица 07 - Още задачи за двоични дървета

```scheme
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

(define bst (make-tree 3
                       (make-tree 1
                                  '()
                                  (make-leaf 2))
                       (make-tree 4
                                  '()
                                  (make-leaf 5))))
```

## Задача 00 - От предния път
Довършете задачите за дървета от предния път.

## Задача 01 - Дърво на извод
Напишете функция, която приема дърво, чиито листа са числа, а останалите възли - аритметични операции. Функцията да пресмята какъв би бил резултата от прилагането на операциите над техните поддървета.

### Пример:
```scheme
(define derivation-tree (make-tree +
                                   (make-tree -
                                              (make-tree +
                                                         (make-leaf 5)
                                                         (make-leaf 7))
                                              (make-leaf 3))
                                   (make-tree *
                                              (make-tree +
                                                         (make-tree /
                                                                    (make-leaf 20)
                                                                    (make-leaf 4))
                                                         (make-leaf 9))
                                              (make-leaf 6))))

(calculate derivation-tree) ; -> 93
```

## Задача 02 - Обръщане на BST
Напишете функция, която сменя знаците на числата във възлите на двоично наредено дърво, като отново запазва наредеността.

### Пример:
```scheme
(inverse bst) ; -> '(-3 (-4 (-5 () ()) ()) (-1 (-2 () ()) ()))
```

## Задача 03 - Огледално дърво
Напишете функция, която проверява дали двоично дърво е огледално. Огледално дърво наричаме такова дърво, на което дясното поддърво има обратно симетрична структура спрямо лявото.

### Пример:
```scheme
;        4
;       / \         
;      1   6
;     /\   /\     
;    7  2 0  8
;    \      / 
;     10   11 

(define mirror (make-tree 4
                          (make-tree 1
                                     (make-tree 7
                                                '()
                                                (make-leaf 10))
                                     (make-leaf 2))
                          (make-tree 6
                                     (make-leaf 0)
                                     (make-tree 8
                                                (make-leaf 11)
                                                '()))))

(mirror? mirror) ; -> #t
(mirror? t) ; -> #f
```

## Задача 04 - Премахване на елемент в BST
Напишете функция, която по подаден елемент и двоично наредено дърво, премахва елемента от дървото, като след това дървото трябва да остане наредено. Ако елементът не е част от дървото, да се върне същото дърво.

### Пример:
```scheme
(bst-remove 3 bst) ; -> '(4 (1 () (2 () ())) (5 () ()))
```
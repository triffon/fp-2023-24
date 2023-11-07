#lang racket

; Функции за работа с двоични дървета

; Проверяваме по дефиницията
(define (tree? t)
  (or (null? t)
      (and (list? t)
           (= (length t) 3)
           (tree? (cadr t))
           (tree? (caddr t)))))

(define root car)

(define left cadr)

(define right caddr)

(define (make-tree root left right)
  (list root left right))

(define empty? null?)

; Едно дърво е листо ако има вида (root '() '()),
; тоест текущия връх няма наследници
(define (leaf? t)
  (and (empty? (left t))
       (empty? (right t))))

; Функции за работа с асоциативни списъци

(define (make-alist fn keys)
  (map (lambda (key)
         (cons key (fn key)))
       keys))

(define (add-assoc key value alist)
  (cons (cons key value)
        alist))

(define (alist-keys alist)
  (map car alist))

;===============;=========================================
;; З А Д А Ч И ;;
;;;;;;;;;;;;;;;;;

; 1. Намира броя на листата в tree.
(define (count-leaves tree)
  (cond ((empty? tree) 0)
        ((leaf? tree) 1)
        (else (+ (count-leaves (left tree))
                 (count-leaves (right tree))))))

; 2. Връща ново дърво,
; в което f е приложена над всеки връх от tree.
(define (map-tree f tree)
  (if (empty? tree)
    '()
    (make-tree (f (root tree))
               (map-tree (left tree))
               (map-tree (right tree)))))

; 3. Връща списък от всички върхове на разстояние n
; от корена на tree.
(define (level n tree)
  (cond ((empty? tree) '())
        ((zero? n) (list (root tree)))
        (else (append (level (- n 1) (left tree))
                      (level (- n 1) (right tree))))))

; Обхождане на дърво, функциите да връщат списък от
; върховете в реда на обхождането им.
;------------------------------------
; 4. корен-ляво-дясно
; (pre-order t) -> (1 2 3 4)
(define (pre-order tree)
  (if (empty? tree)
    '()
    (append (list (root tree))
            (pre-order (left tree))
            (pre-order (right tree)))))

; 5. ляво-корен-дясно
; (in-order t) -> (2 1 3 4)
(define (in-order tree)
  (if (empty? tree)
    '()
    (append (in-order (left tree))
            (list (root tree))
            (in-order (right tree)))))

; 6. ляво-дясно-корен
; (post-order t) -> (2 4 3 1)
(define (post-order tree) (if (empty? tree)
    '()
    (append (post-order (left tree))
            (post-order (right tree))
            (list (root tree)))))

; 7. Обръща 2ката от наследници на всеки връх.
; (root left right) -> (root right left)
(define (flip-tree tree)
  (if (empty? tree)
    '()
    (make-tree (root tree)
               (flip-tree (right tree))
               (flip-tree (left tree)))))


; 8. Връща списък от стойностите на асоциативен списък
(define (alist-values alist)
  (map cdr alist))

; 9. По дадени ключ и асоциативен списък, връща стойността
; от първата намерена двойка с ключа
(define (alist-assoc key alist)
  (cond ((null? alist) '())
        ((equal? (caar alist) key) (cdar alist))
        (else (alist-assoc key (cdr alist)))))

; 10. По даден ключ изтрива първата съответстваща двойка
; със същия ключ
(define (del-assoc key alist)
  (filter (lambda (alist-pair)
            (not (equal? (car alist-pair) key)))
          alist))

; 11. Връща списък от списъци (result . args),
; където args са точно тези елементи x от lst,
; за които f(x) = result
(define (partition-i p? lst)
    (define (op x acc)
      (if (p? x)
        (cons (cons x (car acc))
              (cdr acc))
        (cons (car acc)
              (cons x (cdr acc)))))
      (foldl op '(()) lst))

(define (group-by* f lst)
  (let* ((fhead ((f (car lst))))
         (p? (lambda (x)
               (equal? (f x) fhead)))
         (parts (partition-i p? lst)))
    (if (null? lst)
      '()
      (cons (cons fhead
                  (car parts))
            (group-by* f (cdr parts))))))

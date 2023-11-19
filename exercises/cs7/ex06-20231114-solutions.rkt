#lang racket
(define head car)
(define tail cdr)

; foldl, имплементиран правилно (!)
; в R5RS липсва, докато в racket е объркан редът на аргументите на op
(define (foldl op nv lst)
  (if (null? lst) nv
      (foldl op (op nv (head lst)) (tail lst))))

; Вариант на задача от миналия път
; argmax на функция над списък с елементи
(define (argmax f lst)
  (foldr (lambda (el res)
           (if (> (f el) (f res)) el res))
         (head lst) ; когато няма "тривиален" максимум, просто взимаме първия елемент
         (tail lst)))

(define (argmax* f lst)
  (foldl (lambda (res el)
           (if (> (f el) (f res)) el res))
         (head lst)
         (tail lst)))

(argmax (lambda (x) (remainder x 7)) '(40 41 42 43 44))
(argmax* (lambda (x) (remainder x 7)) '(40 41 42 43 44))

; Зад.3 от контролното, бонус2
; Идея: поддържаме наредена тройка (списък, минимум, максимум),
; към която "натрупваме" максимално ефикасно, а накрая връщаме само списъка.
(define (amplitudes lst)
  (car
   (foldr (lambda (el res)
            (let [(newMin (if res (min el (cadr res)) el))
                  (newMax (if res (max el (cddr res)) el))]
              (cons (cons (- newMax newMin) (if res (car res) '()))
                    (cons newMin newMax))))
            #f ; не може да вземем първия елемент => ползваме специална стойност
            lst)))
(amplitudes '(2 3 10 -2 3 5 -1 3))

; Зад.1
; Тривиално, рекурсивно решение
(define (uniques lst)
  (if (null? lst) '()
      (let [(res (uniques (tail lst)))]
        (if (member (head lst) res)
            res
            (cons (head lst) res)))))
; Ако абстрахираме операцията по условно добавяне, също
; избягваме двукратно извикване на (uniques (tail lst))
(define (cons-if-not x lst)
  (if (member x lst) lst (cons x lst)))

(define (uniques* lst)
  (if (null? lst) '()
      (cons-if-not (head lst)
                   (uniques* (tail lst)))))
; ... но в полученото разпознаваме начина на работа на foldr
(define (uniques** lst)
  (foldr cons-if-not '() lst))
; oт миналия път - полезна за преминаване от ляво към дясно събиране или обратно
(define (flip f)
  (lambda (x y) (f y x)))
(define (uniques*** lst)
  (foldl (flip cons-if-not) '() lst))
; Тривиалното рекурсивно решение, в което само на някои итерации "променяме" резултата
(define (uniques**** lst)
  (define (loop lst res)
    (cond [(null? lst) res]
          [(member (head lst) res) (loop (tail lst) res)]
          [else (loop (tail lst) (cons (head lst) res))]))
  (loop lst '()))

(uniques '(1 2 2 "iei" 1 3 "iei" 'oops))
(uniques* '(1 2 2 "iei" 1 3 "iei" 'oops))
(uniques** '(1 2 2 "iei" 1 3 "iei" 'oops))
(uniques*** '(1 2 2 "iei" 1 3 "iei" 'oops))
(uniques**** '(1 2 2 "iei" 1 3 "iei" 'oops))

; Зад.2
; Модификация с приемане на функция за сравнение по избор
(define (insert cmp val lst)
  (cond [(null? lst) (list val)]
        [(cmp val (head lst)) (cons val lst)]
        [else (cons (head lst)
                    (insert cmp val (tail lst)))]))
; Зад.3, същата модификация
(define (insertion-sort cmp lst)
  (foldl (lambda (res el) (insert cmp el res)) '() lst))

(insertion-sort < '(4 3 6 2 1 8 10))

; Зад.4
; Помощна функция, подобна на двете по-горе: максимум по даден критерий
(define (maximum-by cmp lst)
  (foldl (lambda (res el)
           (if (cmp el res) el res))
         (head lst) ; отново взимаме първия елемент за "временен" максимум
         (tail lst)))
; Дължина на интервал, зададен като наредена двойка
(define (len i) (- (cdr i) (car i)))
; Дали интервалът x е подинтервал на y
(define (sub? x y)
  (and (>= (car x) (car y))
       (<= (cdr x) (cdr y))))
(define (longest-interval-subsets lst)
  (let* [(longest (maximum-by (lambda (x y) (> (len x) (len y))) lst))
         (subs (filter (lambda (x) (sub? x longest)) lst))]
    (insertion-sort (lambda (x y) (< (car x) (car y))) subs)))

(longest-interval-subsets
  '((24 . 25) (90 . 110) (0 . 100) (10 . 109) (1 . 3) (-4 . 2)))

; Зад.5
(define (group-by f lst)
  ; събиране на всички стойности от lst, за които f връща val
  (define (matches val)
    (filter (lambda (x) (equal? (f x) val)) lst))
  (map (lambda (val) (list val (matches val)))
       (uniques (map f lst))))

(group-by even? '(1 2 3 4 5))

; Зад.6
; Стандартни помощни функции
(define (id x) x)
(define (compose f g)
  (lambda (x) (f (g x))))

(define (sq x) (* x x))
(define (1+ x) (+ x 1))
(define (compose* . fns)
  (foldl compose id fns))
((compose* sq 1+ (lambda (x) (* x 2)) 1+) 5)

; Зад.7
; От миналия път
(define (any? p? lst)
  (and (not (null? lst))
       (or (p? (head lst))
           (any? p? (tail lst)))))
(define (zipWith* f . lsts)
  (if (or (null? lsts) (any? null? lsts))
      '()
      (cons (apply f (map head lsts))
            (apply zipWith* f (map tail lsts)))))

(zipWith* max '(1 6 7) '(2 4 9) '(3 5 8 10))

#lang racket

;;; От контролното

;; Да се реализира функция averages, която по подаден непразен списък (x1 x2 … xn) връща списъка (a1 a2 … an), където ai е средно аритметичното на подсписъка (xi … xn).
;;
;; Пример: (averages ‘(10 3 -5 5 3 10 2)) → (4 3 3 5 5 6 2)
;; Бонус: (4 т.) реализирайте функцията чрез единствено линейно обхождане на списъка.
;; Бонус: (6 т.) реализирайте функцията без рекурсия, чрез единствено извикване на foldr.

(define (averages l)
  (caddr
    (foldr
      (lambda (new-element acc)
        (let* ([old-average     (car acc)]
               [old-i           (cadr acc)]
               [old-final-list  (caddr acc)]
               [new-i           (+ old-i 1)]
               [new-average     (/ (+ new-element
                                      (* old-average
                                         old-i))
                                   new-i)]
               [new-final-list  (cons new-average
                                      old-final-list)])
          (list new-average new-i new-final-list
           (list (exp 2) 0 '())
           l))))))

;; (Забавление от междучасието, ignore the fixpoint combinator behind the curtain)

(define Y
  (lambda (f)
    ((lambda (x) (f (lambda (y) ((x x) y))))
     (lambda (x) (f (lambda (y) ((x x) y)))))))

(define fib
  (Y
    (lambda (real-fib)
      (lambda (x)
        (if (< x 2)
          x
          (+ (real-fib (- x 1))
             (real-fib (- x 2))))))))

;;; Задачи

(define (all? p? l)
  (foldr
    (lambda (x acc)
      (if (not (p? x))
        #f
        acc))
    #t
    l))

;; Напишете функция (matrix-ref m i j), която връща елемент на позиция i, j в матрицата m. Пример: (matrix-ref '((1 2 3) (4 5 6) (7 8 9)) 2 1) -> 8

(define (at l i)
  (cond
    [(null? l) 'error]
    [(= i 0)   (car l)]
    [#t        (at (cdr l) (- i 1))]))

(define (matrix-ref m i j)
  (at (at m i) j))

;; Напишете функция (mat? m), която валидира, че m е матрица (всички елементи числа); редовете са с равни дължини)

(define (mat? m)
  (and (list? m)
       (not (null? m))
       (all? list? m)
       (let ([length1 (length (car m))])
         (and (not (= length1 0))
              (all? (lambda (row)
                      (= (length row)
                         length1))
                    (cdr m))))))

;; Напишете функция (delete-column i m), която връща матрица, представляваща m без i-тата си колона:

(define (delete-n i l)
  (cond
    [(null? l) 'error]
    [(= i 0)   (cdr l)]
    [#t        (cons (car l)
                     (delete-n (- i 1)
                               (cdr l)))]))

(define (delete-column i m)
  (map (lambda (l)
         (delete-n i l))
       m))

;; (delete-column 1 '((1 2 3 5)
;;                    (6 7 8 0)
;;                    (5 6 3 4)))
;; => '((1 3 5)
;;      (6 8 0)
;;      (5 3 4))

;; Напишете функция (transpose m), която транспонира матрица.

;; (transpose '((1 2 3) (4 5 6) (7 8 9))) ;; => '((1 4 7) (2 5 8) (3 6 9))

(define (transpose m)
  (if (null? (car m))
    '()
     (cons (map car m)
           (transpose (map cdr m)))))

;; за хитрушите

(define (transpose2 m)
  (apply map list m))

;; за още по-големите хитруши, вижте по-долу

;;; apply / vararg функции

;; apply взима функция, оптионално първи аргументи и лист с аргументи и вика функцията с всички аргументи

;; (apply + 1 2 3 '(4 5 6)) => (+ 1 2 3 4 5 6)

;; plus със специалния синтаксис за vararg функции

(define (plus . l)
  (if (null? l)
    0
    (+ (car l)
       (apply plus (cdr l)))))

;; plus директно с листи, for comparison

(define (plus2 l)
  (if (null? l)
    0
    (+ (car l)
       (plus2 (cdr l)))))

;; Колкото и да е аргумента:

(define (f0 . args)
  'nil)

(define l0
  (lambda args
    'nil))

;; Поне два аругмента

(define (f2 arg1 arg2 . args)
  'nil)

(define l2
  (lambda (arg1 arg2 . args)
    'nil))

;;; За най-големите хитруши

;; в индустрията я има идеята за т.нар. "partial application":
;; да дадем част от аргументите на функция и да получим нова такава, която очаква остатъка от аргументите и пуска оригиналната функция с всички аргументи накуп, примерно:

;; ((partial + 1 2) 3 4) == (+ 1 2 3 4)

;; (define sum (partial foldr + 0))
;; вместо
;; (define sum (lambda (l) (foldr + 0 l))

;;; NOTE: БОНУС ТОЧКИ (2т.): имплементирайте partial
(define partial
  'nil)

;; Имайки partial, може да се доизхитрим и да направим transpose ето така:

(define transpose3
  (partial apply map list))

;;; NOTE: ОЩЕ БОНУС ТОЧКИ (2т.): за големите хитруши, имплементирайте vararg версия на map и zipWith (помислете какво биха правили ако им дадем множество списъци, върху които да работят)

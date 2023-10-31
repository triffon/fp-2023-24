#lang racket
; Зад.0
(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a)
          (accumulate op nv (next a) b term next))))
          
(define (accumulate-i op nv a b term next)
  (if (> a b) nv
      (accumulate-i op (op nv (term a)) (next a) b term next)))

; Полезни помощни функции
(define (id x) x)
(define (++ x) (+ x 1))
; Зад.1
(define (!! n)
  (accumulate-i *
                1
                (if (odd? n) 1 2)
                n
                id
                (lambda (i) (+ i 2))))

; Зад.2
(define (fact n)
  (accumulate-i * 1 1 n id (lambda (i) (+ i 1))))
;(define (nchk n k)
;  (/ (fact n) (fact k) (fact (- n k))))
(define (nchk n k)
  (accumulate-i *
                1
                0
                (- k 1)
                (lambda (i) (/ (- n i) (- k i)))
                ++))

; От миналия път
(define (const c)
  (lambda (i) c))
; Зад.3
; Трябват ни n на брой двойки -> функцията term
; ще върне по една за всяко число от 1 до n
(define (2^ n)
  (accumulate-i * 1 1 n (const 2) ++))

; Зад.4
; Проблем: нямаме short circuit и accumulate винаги
; ще обходим целия интервал, дори да можем да вземем
; решение по-рано (напр. ако още за първия елемент p? върне #f)
(define (all? p? a b)
  (accumulate-i (lambda (x y) (and x y)) #t a b p? ++))
; Аналогичен проблем и тук
(define (any? p? a b)
  (accumulate-i (lambda (x y) (or x y)) #f a b p? ++))

; Зад.5
(define (sum-powers k n)
  (accumulate-i + 0 1 n id (lambda (i) (* i k))))

; Зад.6
; Тук ползваме term за "филтриране" - подменя стойностите, които не искаме,
; с нули, знаейки че няма да повлияят на окончателната сума.
(define (divisors-sum n)
  (accumulate-i +
                0
                1
                n
                (lambda (i)
                  (if (zero? (remainder n i)) i 0))
                ++))

; Зад.7
(define (count p? a b)
  (accumulate-i + 0 a b (lambda (i) (if (p? i) 1 0)) ++))

; Зад.8
(define (prime? n)
  (and (> n 1)
       (all? (lambda (i) (> (remainder n i) 0)) 2 (sqrt n))))

; Зад.9
; Тук "насъбираме" n-те копия на функцията f
(define (compose f g)
  (lambda (x) (f (g x))))
(define (repeat n f)
  (accumulate compose id 1 n (const f) ++))

; Зад.10
(define dx 0.0001)
(define (derive f)
  (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))
; Тук няма промяна :)
(define (derive-n n f)
  ((repeat n derive) f))

; Моделираме (derive (derive (derive f))) -> насъбирането на
; текущия член и резултата от останалите е просто прилагане
; на функция над аргумент
(define (apply f x) (f x))
(define (derive-n* n f)
  (accumulate apply f 1 n (const derive) ++))
; Забележка: операцията прилагане на функция не е комутативна
; => ако искаме да ползваме итеративен процес, трябва да се съобразим
(define (flip f)
  (lambda (x y) (f y x)))
(define (derive-n** n f)
  (accumulate-i (flip apply) f 1 n (const derive) ++))

; Зад.11
; Когато нямаме очевиден неутрален елемент, си взимаме първия от редицата
; и обхождаме само останалите
(define (argmax f a b)
  (accumulate-i (lambda (x y) (if (>= (f x) (f y)) x y))
                a (+ a 1) b id ++))

; Зад.12
; Същата идея от зад.5 - правим floor(log10(n)) итерации
(define (count-digits n)
  (accumulate-i + 0 1 n (const 1) (lambda (i) (* i 10))))
              
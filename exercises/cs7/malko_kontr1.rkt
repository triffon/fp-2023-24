#lang racket
(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a)
          (accumulate op nv (next a) b term next))))
          
(define (accumulate-i op nv a b term next)
  (if (> a b) nv
      (accumulate-i op (op nv (term a)) (next a) b term next)))

; Стандартни помощни функции
(define (++ x) (+ x 1))
(define (sq x) (* x x))
; Дължина на вектор с координати (x,y)
(define (pyth x y) (sqrt (+ (sq x) (sq y))))
; Поточково събиране на две наредени двойки
(define (+. p1 p2) (cons (+ (car p1) (car p2))
                         (+ (cdr p1) (cdr p2))))

; Зад.1
; Идея: можем да сметнем отделно преместването по X със
; (accumulate-i + 0 1 n (lambda (i) (car (f i)) ++) и по Y със същото, но с cdr.
; Можем и с едно обхождане да насъберем целите наредени двойки.
(define (destination f n)
  (accumulate-i +. (cons 0 0) 1 n f ++))

; Зад.2
; Тук разстоянието не зависи от началната точка, само общите дължини
; на векторите, задаващи движенията на всяка стъпка.
(define (path-length f n)
  (define (p i) (pyth (car (f i)) (cdr (f i))))
  (accumulate-i + 0 1 n p ++))

; Връща функция, аналогична на +.,
; но смятаща резултата след ограничаване.
; Това е една интерпретация на "ограничаването", при която
; дали по едната ос е ограничено не зависи от другата ос.
(define (|| bounds)
  (define (clamp x b) (min (max (- b) x) b))
  (lambda (p1 p2)
    (let [(res (+. p1 p2))]
      (cons (clamp (car res) (car bounds))
            (clamp (cdr res) (cdr bounds))))))

; Зад.3
; Отново можеше да имаме отделни функции за X и Y.
(define (destination* f n bounds)
  (accumulate-i (|| bounds) (cons 0 0) 1 n f ++))

; Бонус
; Идея: понеже дължината зависи от текущата позиция,
; "поддържаме" на всяка итерация и позицията, и досегашното разстояние.
; Накрая получаваме едновременно крайната позиция и общата дължина.
; Отново, това е една интерпретация на ограничаването.
; Идеята винаги е същата, но сметките са по-сложни.
(define (path-length* f n bounds)
  (define (op res p)
    (let [(p* ((|| bounds) (car res) p))]
      (cons p* (+ (cdr res)
                  (pyth (- (caar res) (car p*))
                        (- (cdar res) (cdr p*)))))))
  (cdr (accumulate-i op (cons (cons 0 0) 0) 1 n f ++)))

; Тест
(define (path1 i)
  (cond [(= i 1) (cons -7 2)]
        [(= i 2) (cons 3 -3)]
        [(= i 3) (cons 1 5)]))
(destination path1 3)
(path-length path1 3)
(destination* path1 3 (cons 5 4))
(path-length* path1 3 (cons 5 4))
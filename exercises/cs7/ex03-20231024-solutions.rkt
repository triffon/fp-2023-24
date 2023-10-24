#lang racket

; Зад.0
(define (const c)
  (lambda (x) c))

; Пример за функция от по-висок ред на C++:
; auto const_(int c) {
;     return [=](int x) { return c; }; // това е ламбда :)
; }
;
; auto forever21 = const_(21);
; const int x = forever21(5);
; assert(x == 21);

(define (flip f)
  (lambda (x y) (f y x)))

(define (complement p)
  (lambda (x) (not (p x))))

(define (compose f g)
  (lambda (x) (f (g x))))

; Зад.1
(define (id x) x)
; repeat е _операция_, която приема обект и връща
; обект от същия тип - в случая, едноместна функция.
; Няма нужда да пишем ламбди на ръка, имаме всички
; механизми за работа с тези обекти, функциите
(define (repeat n f)
  (if (= n 0) id
      (compose f (repeat (- n 1) f))))
      ;(lambda (x) (f ((repeat (- n 1) f) x)))))

; Зад.2
; Ако ползваме производни от ~3-ти ред и нагоре,
; е добре dx да е по-голямо
(define dx 0.000001)
; Това също е _операция_, приемаща обект (числова функция)
; и връщаща пак обект (числова функция)
(define (derive f)
  (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))

; Зад.3
; Тук ползваме repeat като функция от още по-висок ред:
; приема операция (в случая над числови функции) и връща
; пак операция (над числови функции).
; Получената операция просто прилагаме над дадената числова функция
; и получавам резултат - търсената числова функция :)
(define (derive-n n f)
  ((repeat n derive) f))
  ;(if (= n 0) f
  ;    (derive (derive-n (- n 1) f))))

; Зад.4
(define (twist k f g)
  (repeat (/ k 2) (compose f g)))

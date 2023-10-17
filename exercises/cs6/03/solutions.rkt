#lang racket

; Идентитета не е част от racket, може да ви потрябва :)
(define (id x) x)

; 1. Да се напише итеративна функция, която за дадени:
; - [from, to] - интервал от числа
; - term - функция над цели числа
; - op - бинарна операция над с-ти получени с term(x)
; - acc - начална стойност (дефинирана за op)
; Пресмята натрупаната стойност в acc, получена чрез
; обхождане на интервала [from, to] с операция op
; над стойностите получени чрез term(x)
; (x е елемент от интервала)
(define (accumulate from to term op acc)
  (if (> from to)
      acc
      (accumulate (+ from 1)
                  to
                  term
                  op
                  (op acc (term from)))))


; Реализирайте следните функции чрез accumulate:
;-----------------------------------------------
; 2. Факториел чрез accumulate.
(define (fact n)
  (accumulate 1 n id * 1))

; 3. Проверява дали даден предикат е верен
; за всички числа в даден интервал.
(define (for-all? from to p?)
  (define (and2 a b)
    (and a b))
  (accumulate from to p? and2 #t))

; 4 .Тя проверява дали някое число в даден интервал
; изпълнява даден предикат.
(define (exists? from to p?)
  (define (or2 a b)
    (or a b))
  (accumulate from to p? or2 #f))

; 5. Намира броя на числата в интервал,
; които изпълняват даден предикат.
(define (count-p from to p?)
  (define (succ-if-p acc x)
    (if (p? x) (+ acc 1) acc))
  (accumulate from to id succ-if-p 0))

; 6. Проверява дали дадено число е просто
(define (prime? p) 'TODO)

; 7. Намира броя на простите числа в даден интервал
(define (primes from to) 'TODO)

; 8. Проверява дали в целочисления интервал [a,b]
; съществуват две различни цели числа x и y, такива че:
; f(x) = g(x) и f(y) = g(y)
; Примери:
;   (meet-twice? id (lambda (x) (- x)) -3 1) -> #f
;   (meet-twice? id sqrt 0 5) -> #t (за 0 и 1)
(define (meet-twice? f g a b)
  (define (p? x) (= (f x) (g x)))
  (define (succ-if-p acc x)
    (if (p? x)
        (+ acc 1)
        acc))
  (<= 2
      (accumulate a b id succ-if-p 0)))

; 9. Обръща записа на дадено естествено число
(define (reverse-digits n)
  (define (op acc x)
    (+ (* acc 10)
       (remainder (quotient n (expt 10 x))
                  10)))
  (accumulate 0
              (floor (log n 10))
              id
              op
              0))

; 10. Намира броят на палиндромите в интервала [a,b]
(define (count-palindromes a b)
  (define (is-palindrome? n)
    (= n (reverse-digits n)))
  (count-p is-palindrome? a b))

; 11. Намира средната цифра на записа на дадено число.
; Ако n има четен брой цифри, функцията връща -1.
; Примери:
;   (middle-digit 452) -> 5
;   (middle-digit 4712) -> -1

; sol 1: cut half from the right then get last digit
(define (middle-digit n)
  (define num-len (+ 1 (floor (log n 10))))
  (define (op acc x)
    (quotient acc 10))
  (if (even? num-len)
      -1
      (remainder
       (accumulate 1
                   (quotient num-len 2)
                   id
                   op
                   n)
       10)))

; sol 2: cut from both sides until only the middle is left
(define (middle-digit-2.0 n)
  (define num-len (+ 1 (floor (log n 10))))
  (define (op acc x)
    (remainder (quotient acc 10) ; 1234
               (expt 10 (- (floor (log acc 10)) 1))))
  (if (even? num-len)
      -1
      (accumulate 1
                  (quotient num-len 2)
                  id
                  op
                  n)))

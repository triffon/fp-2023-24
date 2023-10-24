#lang racket

; От миналия път, пример за удобството на квадратните скоби
; когато разглеждаме повече от два случая.
(define (fib n)
  (cond [(= n 0) 0]
        [(= n 1) 1]
        [else (+ (fib (- n 1)) (fib (- n 2)))]))

; Зад.6
(define (prime? n)
  (define (loop i)
    (cond [(> i (sqrt n)) #t]
          [(zero? (remainder n i)) #f]
          [else (loop (+ i 1))]))
  ;(if (= n 1) #f
  ;    (loop 2))
  (and (> n 1) (loop 2)))

; Зад.7
(define (increasing? n)
  ; Внимание - тези стойности се пресмятат винаги,
  ; дори в случаите в които не трябват (!)
  ; Оценяването се случва _преди_ евентуалните проверки
  (let [(last (remainder n 10))
        (rest (quotient n 10))]
    (cond [(< n 10) #t]
          [(<= last (remainder rest 10)) #f]
          [else (increasing? rest)])))

; Зад.8
; Развитие на стандартната идея, при която на всяка итерация
; делим числото на две и остатъкът ни казва дали поредният бит
; трябва да е вдигнат.
(define (toBinary n)
  ; Инварианта: на k-тата итерация pow==2^k
  (define (loop n res pow)
    (cond [(= n 0) res]
          [(zero? (remainder n 2))
           (loop (quotient n 2) res (* pow 10))]
          [else
           (loop (quotient n 2) (+ res (* 1 pow)) (* pow 10))]))
  (loop n 0 1))

; Зад.9
; Аналогичен подход и за обратната трансформация
(define (toDecimal n)
  (define (loop n res pow)
    (cond [(= n 0) res]
          [(zero? (remainder n 10)) ; тази клауза всъщност е ненужна :)
           (loop (quotient n 10) res (* pow 2))]
          [else
           (loop (quotient n 10) (+ res (* (remainder n 10) pow)) (* pow 2))]))
  (loop n 0 1))

; Зад.10
(define (sq x) (* x x))
(define (fast-exp x n)
  (cond [(= n 0) 1]
        [(even? n) (sq (fast-exp x (/ n 2)))]
        [else (* x (sq (fast-exp x (quotient n 2))))]))


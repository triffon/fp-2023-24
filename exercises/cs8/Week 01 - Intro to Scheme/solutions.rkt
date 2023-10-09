(define a 1)
(+ a 3 6 (- 7 8) -6)
(quotient a 3)
(remainder a 3)
(max a 3 7 4)
(expt 3 4)
(sqrt 9)
(define str "hello world")
(define true #t)
(if (= a 5)
    (* a 5)
    (/ a 5))
(cond ((= a 5) #t)
      ((> a 5) "larger")
      (else "smaller"))
(zero? a)
;(= "hello" "hello")
(eqv? "hello" "hello")
(define (f a b)
  (+ a (* 2 b)))
(f 2 4)
; and or not

(define (middle a b c)
  (- (+ a b c)
     (max a b c)
     (min a b c)))

(define (super-number a b c)
  (+ (* (max a b c)
        (min a b c))
     (middle a b c)))

(define (reverse-3-digit-number n)
  (+ (* 100 (remainder n 10))
     (* 10 (remainder (quotient n 10) 10))
     (quotient n 100)))

(define (divisible-by? d n)
  (= (remainder d n) 0))

(define (leap? year)
  (or (and (divisible-by? year 4)
           (not (divisible-by? year 100)))
      (divisible-by? year 400)))

(define (factorial n)
  (if (= 0 n) 1
      (* n (factorial (- n 1)))))

(define (my-gcd a b)
  (cond ((= a b) a)
        ((> a b) (my-gcd (- a b) b))
        (else (my-gcd a (- b a)))))





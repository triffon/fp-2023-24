(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (accumulate-i op nv a b term next)
  (if (> a b) nv
      (accumulate-i op (op nv (term a)) (next a) b term next)))


(define (id x) x)
(define (1+ x) (+ x 1))

(define (p n x)
  (define (term i) (* (- (1+ n) i) (expt x i)))
  (accumulate + 0 0 n term 1+))

(p 3 5)

(define (p n x)
  (define (op u v) (+ (* u x) v))
  (accumulate op 0 1 (1+ n) id 1+))

(p 3 5)

(define (p n x)
  (define (op u v) (+ (* v x) u))
  (accumulate op 0 1 (1+ n) id 1+))

(p 3 5)

(define (p n x)
  (define (op u v) (+ (* u x) v))
  (accumulate-i op 0 1 (1+ n) id 1+))

(p 3 5)

;; (define (accumulate op nv a b term next)
(define (fact n)
  (accumulate * 1 1 n (lambda (i) i) 1+))

(define (pow x n)
  (accumulate * 1 1 n (lambda (i) x) 1+))

(define (myexp x n)
  (accumulate + 0 0 n (lambda (i) (/ (pow x i) (fact i))) 1+))

(define (myexp x n)
  (accumulate (lambda (u v) (1+ (* u v))) 1 1 n (lambda (i) (/ x i)) 1+))

(define (exists? p? a b)
  (accumulate (lambda (u v) (or u v)) #f a b p? 1+))

(define (exists? p? a b)
  (and (<= a b) (or (p? a) (exists? p? (1+ a) b))))

(define (twice f)
  (lambda (x) (f (f x))))

(define (derive f dx)
  (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))

(define (repeated f n)
  (lambda (x)
    (if (= n 0) x
        (f ((repeated f (- n 1)) x)))))

(define (pow x n)
  (if (= n 0) 1
      (* x (pow x (- n 1)))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (twice f)
  (compose f f))

(define (repeated f n)
  (if (= n 0) id
      (compose f (repeated f (- n 1)))))

(define (pow x n)
  (accumulate * 1 1 n (lambda (i) x) 1+))

(define (repeated f n)
  (accumulate compose id 1 n (lambda (i) f) 1+))

(define (derive-n f n dx)
  (if (= n 0) f
      (derive (derive-n f (- n 1) dx) dx)))

(define (derive-n f n dx)
  ((repeated (lambda (h) (derive h dx)) n) f))

(define my#t (lambda (x y) x))
(define my#f (lambda (x y) y))
(define (my-if b x y) ((b x y)))
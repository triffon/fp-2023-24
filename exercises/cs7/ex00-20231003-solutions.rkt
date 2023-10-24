(define x (+ 2 3))
(define foo (* x x x))

(define (sq x) (* x x))

(define (my-abs x)
  (if (> x 0)
      x
      (- x)))

(define (fig1 x y)
  (if (>= y 0)
      (<= (+ (sq x) (sq y)) 4)
      (and (>= y -2) (<= (abs x) 1))))

(define (fig2 x y)
  ; Проверява дали (x,y) е в квадрат със страна 1
  ; и долен ляв ъгъл (sx,sy)
  (define (inSq? sx sy)
    (and (>= x sx) (<= x (+ sx 1))
         (>= y sy) (<= y (+ sy 1))))
  (or (inSq? 0 0)
      (inSq? 1 1)
      (inSq? 2 2)))

(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))











(define (fact n)
  (if (= n 0) 1
      (* n (fact (- n 1)))))

(define (for n r i)
  (if (<= i n)
      (for n (* r i) (+ i 1))
      r))

(define (fact n)
  (for n 1 1))

(fact 4)

(define (sq x) (* x x))

(define (dist x1 y1 x2 y2)
  (let* ((dx (- x2 x1))
         (dy (- y2 y1)))
   (sqrt (+ (sq dx) (sq dy)))))

(define (area x1 y1 x2 y2 x3 y3)
  (let* ((a (dist x1 y1 x2 y2))
         (b (dist x2 y2 x3 y3))
         (c (dist x3 y3 x1 y1))
         (p (/ (+ a b c) 2)))
   (sqrt (* p (- p a) (- p b) (- p c)))))

(define (f x)
  (let* ((a (+ x 2))
         (b (- a 3)))
    (+ a b)))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

(define (fib n)
  (define (iter i fi fi-1)
    (if (= i n) fi
        (iter (+ i 1) (+ fi fi-1) fi)))
  (if (= n 0) 0
      (iter 1 1 0)))
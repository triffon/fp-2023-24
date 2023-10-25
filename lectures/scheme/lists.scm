(define (id x) x)
(define (n+ n) (lambda (i) (+ i n)))

(define (length l)
  (if (null? l) 0
      (+ 1 (length (cdr l)))))

(define (list-tail l n)
  (if (= n 0) l
      (cdr (list-tail l (- n 1)))))

(define (repeated f n)
  (lambda (x)
    (if (= n 0) x
        (f ((repeated f (- n 1)) x)))))

(define (list-tail l n)
  ((repeated cdr n) l))

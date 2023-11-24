(define (foldr op nv l)
  (if (null? l) nv
      (op (car l) (foldr op nv (cdr l)))))

(define (foldl op nv l)
  (if (null? l) nv
      (foldl op (op nv (car l)) (cdr l))))

(define (filter p? l)
  (foldr (lambda (x r) (if (p? x) (cons x r) r)) '() l))

(define (all? p? l)
  (foldr (lambda (x r) (and (p? x) r)) #t l))

(define (matrix? m)
  (and (list? m)
       (not (null? m))
       (all? list? m)
       (all? (lambda (row) (= (length row) (length (car m)))) m)))

(define m '((1 2 3) (4 5 6)))

(define get-rows length)
(define (get-columns m) (length (get-first-row m)))

(define get-first-row car)
(define (get-first-column m) (map car m))

(define del-first-row cdr)
(define (del-first-column m) (map cdr m))

(define get-row list-ref)
(define (get-column m i) (map (lambda (row) (list-ref row i)) m))

(define (transpose m)
  (if (null? (car m)) '()
      (cons (get-first-column m) (transpose (del-first-column m)))))

(define (transpose m)
  (apply map list m))

(define (+-vectors v1 v2)
  (map + v1 v2))
  
(define (+-matrices m1 m2)
  (map +-vectors m1 m2))

(define (*-vectors v1 v2)
  (apply + (map * v1 v2)))

(define (*-matrices m1 m2)
  (map (lambda (row)
       (map (lambda (column)
              (*-vectors row column))
            (transpose m2)))
  m1))


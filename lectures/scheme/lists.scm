(define (id x) x)
(define (n+ n) (lambda (i) (+ i n)))

(define (length l)
  (if (null? l) 0
      (+ 1 (length (cdr l)))))

(define (length l)
  (foldr (lambda (x r) (1+ r)) 0 l))

(define (list-tail l n)
  (if (= n 0) l
      (cdr (list-tail l (- n 1)))))

(define (repeated f n)
  (lambda (x)
    (if (= n 0) x
        (f ((repeated f (- n 1)) x)))))

(define (list-tail l n)
  ((repeated cdr n) l))

(define (mem equality? x l)
  (and (not (null? l))
    (or (and (equality? x (car l)) l) (mem equality? x (cdr l)))))

(define (mem equality? y l)
  (foldr (lambda (x r) (or r (equality? x y))) #f l))

(define (memq x l)   (mem eq? x l))
(define (memv x l)   (mem eqv? x l))
(define (member x l) (mem equal? x l))

(define (from-to a b)
  (if (> a b) '()
      (cons a (from-to (+ a 1) b))))

(define (collect a b next)
  (if (> a b) '()
      (cons a (collect (next a) b next))))

(define 1+ (n+ 1))

(define (from-to a b) (collect a b 1+))

(define (append l1 l2)
  (if (null? l1) l2
      (cons (car l1) (append (cdr l1) l2))))

(define (append l1 l2)
  (foldr cons l2 l1))

(define (reverse l)
  (if (null? l) l
      (append (reverse (cdr l)) (list (car l)))))

(define (reverse l)
  (define (helper l result)
    (if (null? l) result
        (helper (cdr l) (cons (car l) result))))
  (helper l '()))

(define (snoc x r) (append r (list x)))

(define (reverse l)
  (if (null? l) l
      (snoc (car l) (reverse (cdr l)))))

(define (reverse l)
  (foldr snoc '() l))

(define (reverse l)
  (foldl (lambda (r x) (cons x r)) '() l))

(define (map f l)
  (if (null? l) l
      (cons (f (car l)) (map f (cdr l)))))

(define (filter p? l)
  (cond ((null? l) l)
        ((p? (car l)) (cons (car l) (filter p? (cdr l))))
        (else (filter p? (cdr l)))))

(define (foldr op nv l)
  (if (null? l) nv
      (op (car l) (foldr op nv (cdr l)))))

(define (map f l)
  (foldr (lambda (x r) (cons (f x) r)) '() l))

(define (filter p? l)
  (foldr (lambda (x r) (if (p? x) (cons x r) r)) '() l))

(define (accumulate op nv term a next b)
  (foldr op nv (map term (collect a b next))))

(define (foldl op nv l)
  (if (null? l) nv
      (foldl op (op nv (car l)) (cdr l))))

(define (maximum l)
  (foldr max (car l) l))

;; (define (foldr1 op l)
;;  (foldr op (last l) (without-last l)))

;; l трябва да е непразен
(define (foldr1 op l)
  (if (null? (cdr l)) (car l)
      (op (car l) (foldr1 op (cdr l)))))

(define (foldl1 op l)
  (foldl op (car l) (cdr l)))

;; (append '(1 2 3) '(4 5 6) '(7 8 9)) --> (cons 1 (append '(2 3) '(4 5 6) '(7 8 9))
;; ls --> ((1 2 3) (4 5 6) (7 8 9))
;; first --> (1 2 3)
;; result --> (1 2 3 4 5 6 7 8 9)
;; --> ((1 2 3) ....)

;; (append '() '(1 2 3) '(4 5 6) '(7 8 9))
;; ls --> (() (1 2 3) (4 5 6) (7 8 9))
;; (cdr ls) --> ((1 2 3) (4 5 6) (7 8 9))

(define (append . ls)
  (if (null? ls) '()
      ; ls има поне един елемент
      (let ((first (car ls)))
        (if (null? first) (apply append (cdr ls))
            ; first има поне един елемент
            (cons (car first) (apply append (cdr first) (cdr ls)))))))
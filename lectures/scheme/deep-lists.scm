(define (atom? x) (and (not (null? x)) (not (pair? x))))

(define dl '((1 (2)) (((3) 4) (5 (6)) () (7)) 8))

(define (count-atoms dl)
  (cond ((null? dl) 0)
        ((atom? dl) 1)
        (else (+ (count-atoms (car dl)) (count-atoms (cdr dl))))))

(define (count-atoms dl)
  (deep-foldr + 0 (lambda (x) 1) dl))

(define (flatten dl)
  (cond ((null? dl) '())
        ((atom? dl) (list dl))
        (else (append (flatten (car dl)) (flatten (cdr dl))))))

(define (flatten dl)
  (deep-foldr append '() list dl))

(define (snoc x r) (append r (list x)))

(define (deep-reverse dl)
  (cond ((null? dl) '())
        ((atom? dl) dl)
        (else (snoc (deep-reverse (car dl)) (deep-reverse (cdr dl))))))

(define (id x) x)

(define (deep-reverse dl)
  (deep-foldr snoc '() id dl))

(define (deep-foldr op nv term dl)
  (cond ((null? dl) nv)
        ((atom? dl) (term dl))
        (else (op (deep-foldr op nv term (car dl)) (deep-foldr op nv term (cdr dl))))))

(define (foldr op nv l)
  (if (null? l) nv
      (op (car l) (foldr op nv (cdr l)))))

(define (deep-foldr op nv term dl)
  (if (atom? dl) (term dl)
      (foldr op nv (map (lambda (x) (deep-foldr op nv term x)) dl))))

(define (evali x) (eval x (interaction-environment)))
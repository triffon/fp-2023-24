(define (atom? x) (and (not (null? x)) (not (pair? x))))

(define dl '((1 (2)) (((3) 4) (5 (6)) () (7)) 8))

(define (count-atoms dl)
  (cond ((null? dl) 0)
        ((atom? dl) 1)
        (else (+ (count-atoms (car dl)) (count-atoms (cdr dl))))))

(define (flatten dl)
  (cond ((null? dl) '())
        ((atom? dl) (list dl))
        (else (append (flatten (car dl)) (flatten (cdr dl))))))

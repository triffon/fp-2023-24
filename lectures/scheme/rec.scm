(define fact
  (lambda (n)
    (if (= n 0) 1
        (* n (fact (- n 1))))))

(define (gamma-fact f)
  (lambda (n)
    (if (= n 0) 1
        (* n (f (- n 1))))))

(define (gamma-pow2 f)
  (lambda (n)
    (if (= n 0) 1
        (* 2 (f (- n 1))))))


(define (fact n)
  ((gamma-fact fact) n))

(define (pow2 n)
  ((gamma-pow2 pow2) n))

(define (id x) x)

(define (repeated f n)
  (lambda (x)
    (if (= n 0) x
        (f ((repeated f (- n 1)) x)))))

(define (fact n)
  (((repeated gamma-fact (+ n 1)) id) n))


(define (Y gamma)
  ((lambda (me) (lambda (n) ((gamma (me me)) n))) (lambda (me) (lambda (n) ((gamma (me me)) n)))))

(define fact (Y gamma-fact))
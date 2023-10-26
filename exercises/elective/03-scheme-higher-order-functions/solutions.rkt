#lang racket
(require "../common/testing.rkt")
(require rackunit)
(require rackunit/text-ui)

;;;;;;;;;;;
;; UTILS ;;
;;;;;;;;;;;

(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a)
          (accumulate op nv (next a) b term next))))


(define (accumulate-i op nv a b term next)
  (if (> a b)
      nv
      (accumulate-i
        op
        (op nv (term a))
        (next a)
        b
        term
        next)))

(define (1+ x) (+ x 1))
(define (id x) x)
(define (square x) (* x x))

;;;;;;;;;;;;
;; LAMBDA ;;
;;;;;;;;;;;;

;; 1
(define (o f g)
  (lambda (x) (f (g x))))

(let ([tests `(((,1+
                 ,square)
                 . (lambda (f) (= (f 3) 10)))
               ((,id
                 (lambda (x) (- x 5)))
                 . (lambda (f) (= (f 17) 12))))])
  (gen-test-suite o tests))

;; 2 (tests are at the end)
(define (repeated n f x)
  (if (<= n 0)
      x
      (repeated (- n 1) f (f x))))

;; 3
(define (repeat n f)
  (lambda (x)
    (repeated n f x)))

;;;;;;;;;;;;;;;;
;; ACCUMULATE ;;
;;;;;;;;;;;;;;;;

;; 1
(define (my-count p a b)
  (accumulate-i + 0 a b (lambda (i) (if (p i) 1 0)) 1+))

(let ([tests '(((even? 1 11) . 5)
               (((lambda (x) (< x 10)) 3 42) . 7)
               (((lambda (x) (= x 7)) -10 50) . 1)
               (((lambda (x) (= x 7)) 10 50) . 0))])
  (gen-test-suite my-count tests))

;; 2
(define (any? p a b)
  (accumulate-i (lambda (x y) (or x y)) #f a b p 1+))

(let ([tests '(((even? 3 3) . #f)
               (((lambda (x) (< x 10)) 5 15) . #t)
               (((lambda (x) (< x 10)) 11 15) . #f)
               (((lambda (x) (= x 7)) -10 50) . #t)
               (((lambda (x) (= x 7)) 10 50) . #f))])
  (gen-test-suite any? tests))

;; 3
(define (all? p a b)
  (accumulate-i (lambda (x y) (and x y)) #t a b p 1+))
; alternatively
(define (all?-2 p a b)
  (not (any? (lambda (x) (not (p x))) a b)))

(let ([tests '((((lambda (x) (< x 10)) -100 9) . #t)
               (((lambda (x) (< x 10)) 9 15) . #f)
               (((lambda (x) (not (= x 7))) -10 50) . #f)
               (((lambda (x) (> x 7)) 10 50) . #t))])
  (gen-test-suite all? tests)
  (gen-test-suite all?-2 tests))

;; 4

;; repeated-accum
(define (repeated-accum n f x)
  (accumulate (lambda (_fst snd) (f snd)) x 1 n id 1+))
; and iteratively
(define (repeated-accum-i n f x)
  (accumulate-i (lambda (fst _snd) (f fst)) x 1 n id 1+))


(run-tests
  (test-suite "repeated tests"
    (check-equal?
      (repeated
          10
          (lambda (x) (* x 2))
          1)
      1024)
    (check-equal?
      (repeated 100 1+ 0)
      100)
    (check-equal?
      (repeated 0 1+ 42)
      42)
    )
  'verbose)

(let ([tests '(((10  (lambda (x) (* x 2)) 1 ) . 1024)
               ((100 (lambda (x) (+ x 1)) 0 ) . 100)
               ((0   (lambda (x) (+ x 1)) 42) . 42))])
  (gen-test-suite repeated tests)
  (gen-test-suite repeated-accum tests)
  (gen-test-suite repeated-accum-i tests))

;; repeat-accum
(define (repeat-accum n f)
  (accumulate o id 1 n (lambda (_) f) 1+))
; and iteratively
(define (repeat-accum-i n f)
  (accumulate-i o id 1 n (lambda (_) f) 1+))

(let ([tests `(((10  (lambda (x) (* x 2))) . (lambda (f) (= (f 1) 1024)))
               ((100 ,1+) . (lambda (f) (= (f 0) 100)))
               ((0   ,1+) . (lambda (f) (= (f 42) 42))))])
  (gen-test-suite repeat tests)
  (gen-test-suite repeat-accum tests)
  (gen-test-suite repeat-accum-i tests))


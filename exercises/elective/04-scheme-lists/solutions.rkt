#lang racket
(require "../common/testing.rkt")

;;;;;;;;;;;
;; UTILS ;;
;;;;;;;;;;;
(define (id x) x)

;; 1
(define (len l)
  (if (null? l)
      0
      (+ 1 (len (cdr l)))))

(let ([tests '((('(1 2 3))       . 3)
               (('(1 2 3 4 5 6)) . 6)
               (('())            . 0))])
  (gen-test-suite len tests))

;; 2
(define (any? p l)
  (if (null? l)
      #f
      (or (p (car l)) (any? p (cdr l)))))

(let* ([lt (lambda (x) (lambda (y) (< y x)))]
       [tests `(((odd?     '(2 4 6 0))   . #f)
                ((odd?     '(2 4 6 0 7)) . #t)
                ((odd?     '())          . #f)
                ((,(lt 10) '(10 11 12))  . #f)
                ((,(lt 10) '(9 10 11))   . #t))])
  (gen-test-suite any? tests))

(define (all? p l)
  (not (any?
        (lambda (x)
          (not (p x)))
        l)))

(let* ([gt (lambda (x) (lambda (y) (> y x)))]
       [tests `(((even?   '(2 4 6 0))   . #t)
                ((even?   '(2 4 6 0 7)) . #f)
                ((odd?    '())          . #t)
                ((,(gt 9) '(10 11 12))  . #t)
                ((,(gt 9) '(9 10 11))   . #f))])
  (gen-test-suite all? tests))

;; 3
(define (member? x l)
  (if (null? l)
      #f
      (or
       (equal? x (car l))
       (member? x (cdr l)))))

; or the usual programmer way
(define (member?-via-any? x l)
  (any? (lambda (el) (equal? el x)) l))

(let* ([l1 ''(1 2 3 4 5 6 7 8)]
       [l2 ''(0 (12 13) (21 22))]
       [tests `(((5        ,l1)        . #t)
                ((10       ,l1)        . #f)
                (('(12 13) ,l2)        . #t)
                (('(())    '(() (()))) . #t)
                (('()      '((())))    . #f)
                ((0        ,l2)        . #t))])
  (gen-test-suite member? tests)
  (gen-test-suite member?-via-any? tests))

;; 4
(define (at n l)
  (cond
    [(< n 0)    #f]
    [(null? l)  #f]
    [(= n 0)    (car l)]
    [else       (at (- n 1) (cdr l))]))

(let ([tests '(((2 '(1 2 3)) . 3)
               ((0 '(1 2 3)) . 1)
               ((0 '())      . #f))])
  (gen-test-suite at tests))

;; 5
(define (my-map f l)
  (if (null? l)
      (list)
      (cons
       (f (car l))
       (my-map f (cdr l)))))
; тестовете долу (след reduce)

;; 6
(define (my-filter p l)
  (if (null? l)
      (list)
      (let ([head (car l)]
            [tail-filtered (my-filter p (cdr l))])
        (if (p head)
            (cons head tail-filtered)
            tail-filtered))))

; or if you're like me and love your lambdas :D
; (don't actually do this, it hurts people)
(define (my-filter-2 p l)
  (if (null? l)
      (list)
      (((λ (head)
          (if (p head)
              (λ (tail)
                (cons head tail))
              id))
        (car l))
       (my-filter-2 p (cdr l)))))
; тестовете долу (след reduce)

;; 7
(define (push x l)
  (if (null? l)
      (list x)
      (cons
       (car l)
       (push x (cdr l)))))

(let ([tests `(((4 '(1 2 3)) . '(1 2 3 4))
               ((1 '())      . '(1))
               ((2 '(1))     . '(1 2)))])
  (gen-test-suite push tests))

;; 8
(define (reverse l)
  (define (iter acc rest)
    (if (null? rest)
        acc
        (iter (cons (car rest) acc) (cdr rest))))
  (iter (list) l))

(let ([tests `((('(1 2 3)) . '(3 2 1))
               (('(1))     . '(1))
               (('())      . '())
               (('(1 2 2)) . '(2 2 1)))])
  (gen-test-suite reverse tests))

;; 9
(define (insert x n l)
  (cond
    [(<= n 0) (cons x l)]
    [(null? l) (list x)]
    [else (cons
           (car l)
           (insert x (- n 1) (cdr l)))]))

(let ([tests `(((4  0   '(1 2 3)) . '(4 1 2 3))
               ((1  0   '())      . '(1))
               ((-1 100 '(1 2 3)) . '(1 2 3 -1)))])
  (gen-test-suite insert tests))

;; 10
(define (range a b)
  (if (> a b)
      (list)
      (cons a (range (+ a 1) b))))

(let ([tests `(((0 3) . '(0 1 2 3))
               ((0 0) . '(0))
               ((1 0) . '()))])
  (gen-test-suite range tests))

;; 11
(define (sum l)
  (if (null? l)
      0
      (+
       (car l)
       (sum (cdr l)))))
; тестовете долу (след reduce)

;; 12
(define (reduce op init l)
  (if (null? l)
      init
      (op (car l) (reduce op init (cdr l)))))

; ъъъ, как да тестваме reduce?
; ами чрез имплементациите долу
; (където намираме и липсващите тестове от горните имплементации)

;; *)
(define (rmap f l)
  (reduce
   (lambda (x xs)
     (cons (f x) xs))
   (list)
   l))

;; *)
(define (rfilter p l)
  (reduce
   (lambda (x xs)
     (if (p x)
         (cons x xs)
         xs))
   (list)
   l))

;; *)
(define (rsum l)
  (reduce + 0 l))

;; REDUCE TESTS

;; map tests
(let ([tests '((((lambda (x) (+ x 1))
                 (list 1 2 3 4 5))
                . '(2 3 4 5 6))
               (((lambda (x) (- x 1))
                 (list 1 2 3 4 5))
                . '(0 1 2 3 4))
               (((lambda (x) (* x x))
                 (list 1 2 3 4 5))
                . '(1 4 9 16 25))
               (((lambda (x) (string-append "Number " (number->string x)))
                 (list 1 2 3 4 5))
                . '("Number 1" "Number 2" "Number 3" "Number 4" "Number 5"))
               ((string-length
                 (list "apple" "banana" "cherry"))
                . '(5 6 6))
               (((lambda (x) (string-append "I love " x))
                 (list "apple" "banana" "cherry"))
                . '("I love apple" "I love banana" "I love cherry"))
               (((lambda (x) (string->symbol x))
                 (list "apple" "banana" "cherry"))
                . '(apple banana cherry))
               (((lambda (x) (if (even? x) "Even" "Odd"))
                 (list 2 3 5 7 11 13 17))
                . '("Even" "Odd" "Odd" "Odd" "Odd" "Odd" "Odd"))
               (((lambda (x) (+ x 5))
                 (list 10 20 30 40 50))
                . '(15 25 35 45 55))
               (((lambda (x) (- x 2))
                 (list 10 20 30 40 50))
                . '(8 18 28 38 48))
               (((lambda (x) (* x 2))
                 (list 10 20 30 40 50))
                . '(20 40 60 80 100)))])
  (gen-test-suite my-map tests)
  (gen-test-suite rmap tests))

;; filter tests
(let ([tests `(((even?                '(1 2 3 4 5 6 7 8))  . '(2 4 6 8))
               ((,id                  '(#f #f #t #f))      . '(#t))
               (((lambda (x) (< x 3)) '(0 1 2 3 4 5 6 -1)) . '(0 1 2 -1)))])
  (gen-test-suite my-filter tests)
  (gen-test-suite my-filter-2 tests)
  (gen-test-suite rfilter tests))

;; sum tests
(let ([tests `(((',(range 1 6))         . 21)
               (('())                   . 0)
               (('(-1 5 -2))            . 2)
               (((list 2 4 6 8 17 -20)) . 17))])
  (gen-test-suite sum tests)
  (gen-test-suite rsum tests))
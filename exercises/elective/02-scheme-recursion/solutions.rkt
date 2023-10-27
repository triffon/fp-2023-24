#lang racket
(require "../common/testing.rkt")

;; Util
(define %  remainder)
(define // quotient)

;; 1
(define (sum-digits n)
  (if (= n 0)
      0
      (+ (% n 10)
         (sum-digits (// n 10)))))

(define (sum-digits-iter n)
  (define (iter sum remaining)
    (if (= remaining 0)
        sum
        (iter (+ sum
                 (% remaining 10))
              (// remaining 10))))
  (iter 0 n))

;; ((arg1 arg2 ...) . expected)
(let ([tests '(((12345)      . 15)
               ((1020304050) . 15)
               ((0)          . 0)
               ((1009)       . 10)
               ((1000000)    . 1))])
  (gen-test-suite sum-digits      tests)
  (gen-test-suite sum-digits-iter tests))

;; 2
(define (count-divisors n)
  (define (iter cnt i)
    (if (= i n)
        cnt
        (iter (+ cnt (if (= (% n i) 0)
                         1
                         0))
              (+ i 1))))
  (iter 1 1))

(let ([tests '(((6) . 4))])
  (gen-test-suite count-divisors tests))

;; 3
(define (prime? n)
  (= (count-divisors n)
     2))

(define (prime?-2 n)
  (define (iter i)
    (if (> i (sqrt n))
        #t
        (and (not (= (% n i) 0))
             (iter (+ i 1)))))
  (iter 2))

(let ([tests '(((2) . #t)
               ((3) . #t)
               ((4) . #f)
               ((5) . #t)
               ((6) . #f))])
  (gen-test-suite prime?   tests)
  (gen-test-suite prime?-2 tests))

;; 4
(define (increasing-digits-iter n)
  (define (increasing-last-2-digits m)
    (< (% (// m 10) 10)
       (%     m     10)))
  (define (iter m)
    (if (< -10 m 10)
        #t
        (and (increasing-last-2-digits m)
             (increasing-digits-iter (// m 10)))))
  (iter n))

(let ([tests '(((123) . #t)
               ((132) . #f))])
  (gen-test-suite increasing-digits-iter tests))

;; 5
(define (count-digits-iter n [base 10])
  (define (iter cnt i)
    (if (< (- base) i base)
        cnt
        (iter (+ cnt 1)
              (// i base))))
  (iter 1 n))

;; INFO: errors out on 0
;; (define (count-digits m)
;;  (+ 1 (round (log m 10))))

(define (strip-left-digit m)
  (- m
     (expt 10 (- (count-digits-iter m)
                 1))))

(define (ends-with?-iter-1 n k)
  (define (iter m)
    (cond
      ;; edge-case-ово ни е
      [(and (= k 0) (not (= (% m 10) 0))) #f]
      [(< m k) #f]
      [(= m k) #t]
      [(> m k) (iter (strip-left-digit m))]))
  (iter n))

;; NOTE: `-iter` only because of `count-digits-iter`
(define (ends-with?-iter*-2 n k)
  (= k
     (% n
        (expt 10
              (count-digits-iter k)))))

(define (ends-with?-iter-3 n k)
  (let ([last-n (% n 10)]
        [last-k (% k 10)])
    (cond
      [(< k 10)
       (= last-n last-k)]
      [(not (= last-n last-k))
       #f]
      [else
        (ends-with?-iter-3
         (// n 10)
         (// k 10))])))

(define (ends-with?-lists-4 n k)
  (define (list-digits-reverse m)
    (if (< m 10)
        (list m)
        (cons (% m 10)
              (list-digits-reverse
               (// m 10)))))

  (let ([k-digits (list-digits-reverse k)])
    (equal?
     (take (list-digits-reverse n) (length k-digits))
     k-digits)))

(let ([tests '(((152352363123 2363123) . #t)
               ((152352363123 3023)    . #f)
               ((5            0)       . #f)
               ((0            0)       . #t))])
  (gen-test-suite ends-with?-iter-1  tests)
  (gen-test-suite ends-with?-iter*-2 tests)
  (gen-test-suite ends-with?-iter-3  tests)
  (gen-test-suite ends-with?-lists-4 tests))

;; 5.1
(define (automorphic? n)
  (define ends-with?
    ends-with?-iter*-2)
  (ends-with? (* n n)
              n))

(let ([tests '(((4)  . #f)
               ((5)  . #t)
               ((6)  . #t)
               ((11) . #f))])
  (gen-test-suite automorphic? tests))

;; 6
(define (perfect? n)
  (define (iter sum i)
    (if (= i n)
        sum
        (iter (+ sum (if (= (% n i) 0)
                         i
                         0))
              (+ i 1))))
  (= n (iter 0 1)))

(let ([tests '(((6)        . #t)
               ((8126)     . #f)
               ((33550336) . #t))])
  (gen-test-suite perfect? tests))

;; (and 7 8)
(define (base1-to-base2 base1 base2 n)
  (define (iter acc k i)
    (if (= i 0)
        acc
        (iter (+ acc
                 (* (expt base1 k)
                    (% i base2)))
              (+ k 1)
              (// i base2))))
  (iter 0 0 n))

(define (binary-to-decimal-iter n)
  (base1-to-base2 2 10 n))

(define (decimal-to-binary-iter n)
  (base1-to-base2 10 2 n))

(let ([tests '(((101)       . 5)
               ((10001)     . 17)
               ((101011111) . 351))])
  (gen-test-suite binary-to-decimal-iter tests)
  (gen-test-suite decimal-to-binary-iter
                  (map (lambda (pair)
                         (match pair
                           [(cons (list binary) decimal)
                            (cons (list decimal) binary)]))
                       tests)))

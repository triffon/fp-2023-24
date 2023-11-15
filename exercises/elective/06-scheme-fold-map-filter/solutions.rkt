#lang racket
(require "../common/testing.rkt")

;;;;;;;;;;;;;;;;
;; DISCLAIMER ;;
;;;;;;;;;;;;;;;;
; Тук използваме foldl от стандартната библиотека,
; в която имплементация бинарната операция взима настоящия елемент и акумулатора
; в същия ред като foldr, т.е. наобратно от нашата имплементация от лекции

;; 1
; Няма значение, понеже събирането е комутативно
(define (sum-r l)
  (foldr + 0 l))
(define (sum-l l)
  (foldl + 0 l))

(let ([tests '((('(1 2 3))         . 6)
               (('(1 2 3 4 5 6 7)) . 28)
               (((range 1 101))   . 5050))])
  (gen-test-suite sum-r tests)
  (gen-test-suite sum-l tests))

;; 2
; Тук има значение:
; - с foldr става с едно обхождане
; - s foldl можем да ги получим в обратен ред; иска reverse
;   * алтернативно, можем да използваме append вместо cons, но това е още по-бавно :D
(define (map-r f l)
  (foldr
    (lambda (x acc)
      (cons (f x) acc))
    '()
    l))

(define (map-l-reverse f l)
  (reverse
    (foldl
      (lambda (x acc)
        (cons (f x) acc))
      '()
      l)))

(define (map-l-append f l)
  (foldl
    (lambda (x acc)
      (append acc (list (f x))))
    '()
    l))

; (define (1+ x) (+ x 1))
(define (id x) x)

(let* ([1+ (lambda (x) (+ x 1))]
      ;  [id (lambda (x) x)]
       [sq (lambda (x) (* x x))]
       [tests `(((,1+        '(0 1 2 -5))  . '(1 2 3 -4))
                ((,id        '(0 1 2 -5))  . '(0 1 2 -5))
                ((,sq        '(-2 -1 0 3)) . '(4 1 0 9))
                (((const #t) '(0 1))       . '(#t #t)))])
  (gen-test-suite map-r tests)
  (gen-test-suite map-l-reverse tests)
  (gen-test-suite map-l-append tests))

;; 3
; Тук отново има значение - по подобни причини като с map
(define (filter-r p? l)
  (foldr
    (lambda (x acc)
      (if (p? x)
          (cons x acc)
          acc))
    '()
    l))

(let ([tests '(((even? '(1 2 3 4)) . '(2 4))
               ((odd? '(1 2 3 4 5 6 7)) . '(1 3 5 7)))])
  ; (gen-test-suite sum-r tests)
  (gen-test-suite filter-r tests))

;; 4
(define (foldr1 op l)
  (if (null? (cdr l))
      (car l)
      (op (car l) (foldr1 op (cdr l)))))

(let ([tests '(((+ '(1 2 3)) . 6)
               ((- '(1 2 3 4 5 6 7)) . 4)
               ((cons '(1 2 3 ())) . '(1 2 3)))])
  (gen-test-suite foldr1 tests))

; note that the order of arguments that `op` accepts
; will match the order in which it is expected to accept
; them in the underlying `foldl` implementation
(define (foldl1 op l)
  (foldl op (car l) (cdr l)))

(let ([tests '(((+ '(1 2 3)) . 6)
               ((cons '(() 1 2 3)) . '(3 2 1)))])
  (gen-test-suite foldl1 tests))

;; 5
; тук няма значение, тъй като `1+` е комутативна,
; но предпочитаме foldl, защото е итеративна
(define (len-r l)
  (foldr (lambda (_ acc) (+ 1 acc)) 0 l))

(define (len-l l)
  (foldl (lambda (_ acc) (+ 1 acc)) 0 l))

(let ([tests '((('(1 2 3))      . 3)
               (('())           . 0)
               (((range 1 101)) . 100))])
  (gen-test-suite len-r tests)
  (gen-test-suite len-l tests))

;; 6
; няма значение; `or` е комутативна
(define (any?-r p? l)
  (foldr
    (lambda (x acc)
      (or acc (p? x))) ; first goes `acc` for short circuiting's sake
    #f
    l))

(define (any?-l p? l)
  (foldl
    (lambda (x acc)
      (or acc (p? x))) ; first goes `acc` for short circuiting's sake
    #f
    l))

; test suite copied from last week
(let* ([lt (lambda (x) (lambda (y) (< y x)))]
       [tests `(((odd?     '(2 4 6 0))   . #f)
                ((odd?     '(2 4 6 0 7)) . #t)
                ((odd?     '())          . #f)
                ((,(lt 10) '(10 11 12))  . #f)
                ((,(lt 10) '(9 10 11))   . #t))])
  (gen-test-suite any?-r tests)
  (gen-test-suite any?-l tests))

;; 7
; тук категорично foldl е по-удобно,
; защото има бонус ефекта просто с един cons да постигнем именно обратен ред
(define (reverse-l l)
  (foldl cons '() l))

(let* ([some-list (range 1 101)]
       [tests `((('(1 2 3))    . '(3 2 1))
                (('())         . '())
                ((',some-list) . ',(reverse some-list)))]) ; testing with the built-in reverse, very creative - yes :D
  (gen-test-suite reverse-l tests))

;; 8
; тук foldl е по-удобно, защото искаме да броим елементи от началото на `l`
; admittedly, `take` е доста неудобна за имплементация с fold, просто рекурсийка е по-удобен подход :)
; понеже въртим-сучем, без reverse трудно - не е оптимално с fold :)
(define (take n l)
  (reverse
    (cdr
      (foldl
        (lambda (x acc)
          (let ([remaining-n (car acc)]
                [xs          (cdr acc)])
            (if (> remaining-n 0)
                (cons (- remaining-n 1)
                      (cons x xs))
                (cons 0 xs))))
        (cons n '())
        l))))

(define (take-recursive n l)
  (if (or (<= n 0) (null? l))
      '()
      (cons (car l)
            (take-recursive (- n 1) (cdr l)))))

(let* ([tests `(((3 '(1 2 3 4)) . '(1 2 3))
                ((3 '(1 2))     . '(1 2))
                ((0 '(1 2 3))   . '()))])
  (gen-test-suite take tests)
  (gen-test-suite take-recursive tests))

;; 10
; zipWith
(define (zipWith f l1 l2)
  (if (or (null? l1) (null? l2))
      '()
      (cons (f (car l1) (car l2))
            (zipWith f (cdr l1) (cdr l2)))))

(let* ([tests `(((+ '(1 2 3) '(5 4 2)) . '(6 6 5))
                ((- '(1 2 3) '(1 2 3)) . '(0 0 0))
                ((* '(1 2 3) '(3 2 1)) . '(3 4 3)))])
  (gen-test-suite zipWith tests))

; zip
(define (zip l1 l2)
  (zipWith cons l1 l2))

(let* ([tests `((('(1 2 3) '("a" "b" "c")) . '((1 . "a") (2 . "b") (3 . "c")))
                (('(1 2) '(3 4 5 6 7)) . '((1 . 3) (2 . 4))))])
  (gen-test-suite zip tests))
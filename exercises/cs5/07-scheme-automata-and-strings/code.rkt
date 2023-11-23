#lang racket

; check if l is a list
(define (list? l)
  (if (null? l)
      #t
      (if (pair? l)
          (list? (cdr l))
          #f)))

; checks if p returns true for all elements of l
(define (all? p l)
  (foldr (lambda (x y) (and x y)) #t (map p l)))

(define (sum l)
  (foldr + 0 l))

; counts the number of elements in l for which p returns true
(define (count p l)
  (sum (map (lambda (x) (if (p x) 1 0)) l)))

; counts the number of occurences of el in l
(define (count-occurances el l)
  (count (lambda (other) (equal? other el)) l))

; checks if l contains no duplicate elements
(define (unique? l)
  (all? (lambda (el) (= 1 (count-occurances el l))) l))

(define (keys l)
  (map car l))

(define assoc-empty (list))

(define (assoc? l)
  (and
   (list? l)
   (all? pair? l)
   (unique? (keys l))))

(define (assoc-set l k v)
  (if (null? l)
      (list (cons k v))
      (if (equal? (caar l) k)
          (cons (cons k v) (cdr l))
          (cons (car l) (assoc-set (cdr l) k v)))))

(define (assoc-map f l)
  (map (lambda (p) (cons (car p) (f (cdr p)))) l))

(define (assoc-filter pred l)
  (filter (lambda (p) (pred (cdr p))) l))

(define (assoc-get l k)
  (cdar (filter (lambda (p) (equal? (car p) k)) l)))

(define my-automaton
  (list
   (cons 'delta
         (list
          (cons (cons 'ivan 'b) 'pesho)
          (cons (cons 'ivan 'a) 'gosho)
          (cons (cons 'pesho 'b) 'ivan)
          (cons (cons 'pesho 'a) 'pesho)
          (cons (cons 'gosho 'a) 'gosho)
          (cons (cons 'gosho 'b) 'gosho)))
   (cons 'start 'ivan)
   (cons 'final (list 'pesho))))

(define baba-automaton
  (list
   (cons 'delta
         (list
            (cons (cons 'gosho #\b) 'pesho)
            (cons (cons 'gosho #\a) 'err)
            (cons (cons 'pesho #\b) 'err)
            (cons (cons 'pesho #\a) 'gosho)
            (cons (cons 'err #\a) 'err)
            (cons (cons 'err #\b) 'err)))

   (cons 'start 'gosho)
   (cons 'final (list 'gosho))))

(define (apply-delta delta state symb)
  (assoc-get delta (cons state symb)))

(define (apply-delta* delta state word)
  (define (apply-delta*-help delta state word i)
    (if (>= i (string-length word))
        state
        (apply-delta*-help delta (apply-delta delta state (string-ref word i)) word (+ i 1))))
  
  (apply-delta*-help delta state word 0))

(define (member? x l)
  (not (null? (filter (lambda (other) (equal? other x)) l))))

(define (accepts-word? automaton word)
  (let
      ((finals (assoc-get automaton 'final))
       (start (assoc-get automaton 'start))
       (delta (assoc-get automaton 'delta)))
    (member? (apply-delta* delta start word) finals)))


(define (foldr op null ll)
  (if (null? ll)
      null
      (op (car ll) (foldr op null (cdr ll)))))

(define (flatten ll)
  (foldr append (list) ll))
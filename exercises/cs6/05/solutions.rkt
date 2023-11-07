#lang racket

(define (foldl* op acc lst)
  (if (null? lst)
      acc
      (foldl* op
              (op acc (car lst))
              (cdr lst))))

(define (foldr* op acc lst)
  (if (null? lst)
      acc
      (op (car lst)
          (foldr* op acc (cdr lst)))))

; Използвайте foldl* или foldr:
;-----------------------------
; 1. Намира дължина на списък
(define (length* lst)
  (foldl* (lambda (x _) (+ x 1)) 0 lst))

; 2. Премахва повторенията на елементи в lst
(define (uniques lst)
  (foldr* (lambda (el res)
            (if (member el res) res (cons el res)))
          '()
          lst))

; 3. Проверява дали p? е верен за точно n елемента от lst
(define (sat-n? p? n lst)
  (define (op x acc)
    (if (p? x)
        (+ 1 acc)
        acc))
  (= n (foldr* op 0 lst)))

; 4. Връща списък с елементите на lst, но в обратен ред.
(define (flip binary-op)
  (lambda (x y) (binary-op y x)))

(define (reverse* lst)
  (foldl* (flip cons) '() lst))

; 5. Намира броя на елементите в дълбокия списък lst.
; Тоест lst може да има произволни нива на вложеност.
(define (count-atoms lst)
  (define (op x acc)
    (cond ((null? x) 0)
          ((list? x) (+ acc (count-atoms x)))
          (else (+ acc 1))))
  (foldr* op 0 lst))

; 6. Връща наредена двойка (fst . snd), където
; fst са елементите за които p? е истина
; и snd са тези за които p? е лъжа
(define (partition-i p? lst)
  (define (op x acc)
    (if (p? x)
        (cons (cons x (car acc))
              (cdr acc))
        (cons (car acc)
              (cons x (cdr acc)))))
  (foldl* op '(()) lst))

; (HINT) Използвайте apply:
;--------------------------
; 7. Средно аритметично на много аргументи
(define (avg h . t)
  (/ (apply + (cons h t))
     (length (cons h t))))

; 8. Композиция на на много едноаргументни функции
(define (compose f g)
  (lambda (x) (f (g x))))

(define (compose-all f . gs)
  (if (null? gs)
      f
      (compose f (apply compose-all gs))))

; 9. Конюнкция на много едноместни предикати
(define (conjoint p1? p2?)
  (lambda (x) (and (p1? x) (p1? x))))

(define (conjoint-all p? . preds)
  (if (null? preds)
      p?
      (conjoint p? (apply conjoint-all preds))))

; 10. Като map на много аргументи, но трябва да работи за
; списъци с различни дължини.
(define (any? p? lst)
  (foldr* (lambda (x acc) (or (p? x) acc)) #f lst))

(define (zipWith f . lsts)
  (if (or (null? lsts)
          (any? null? lsts))
      '()
      (cons (apply f (map car lsts))
            (apply zipWith f (map cdr lsts)))))

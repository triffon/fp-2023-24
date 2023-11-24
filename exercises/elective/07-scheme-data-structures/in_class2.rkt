#lang racket

(define the-empty-tree (list))
(define e the-empty-tree)

(define (make-tree root left right)
  (list root left right))
(define mk make-tree)

(define (make-leaf-tree root)
  (mk root e e))
(define mkl make-leaf-tree)

(define t-root car)
(define t-left cadr)
(define t-right caddr)
(define (t-empty? t)
  (equal? t e))

(define (t-leaf? t)
  (and (not (t-empty? t))
       (t-empty? (t-left t))
       (t-empty? (t-right t))))

(define (tree? t)
  (or (t-empty? t)
      (and
        (= 3 (length t))
        (tree? (cadr t))
        (tree? (caddr t)))))

(define example-tree
  (mk 1
    (mkl 2)
    (mk 3
      (mkl 4)
      (mk 5
        e
        (mkl 6)))))

;; 2
(define (collect-pre-order-2 t)
  (if (t-empty? t)
      (list)

          (append
            (list (t-root t))
            (collect-pre-order (t-left t))
            (collect-pre-order (t-right t)))))

(define (collect-pre-order t)
  (if (t-empty? t)
      (list)
      (append
        (list (t-root t))
        (collect-pre-order (t-left t))
        (collect-pre-order (t-right t)))))

(define (collect-in-order t)
  (if (t-empty? t)
      (list)
      (append
        (collect-in-order (t-left t))
        (list (t-root t))
        (collect-in-order (t-right t)))))

(define (collect-post-order t)
  (if (t-empty? t)
      (list)
      (append
        (collect-post-order (t-left t))
        (collect-post-order (t-right t))
        (list (t-root t)))))


;; assoc
(define example-al
  (list
    (cons 1 2)
    (cons 1 "a")
    (cons 2 5)
    (cons -3 "aloda")
    (cons 3.5 1)
    (cons 3 2)))

;; assoc, assv, assq
(define (zipWith f l1 l2)
  (map f l1 l2))

(define (index l)
  (zipWith
    cons
    (range (length l))
    ))

;; presumable without duplicates
(define (put2 k v al [equal? equal?])
  (if (empty? al)
      (list (cons k v))
      (let* ([head (car al)]
             [tail (cdr al)]
             [k1 (car head)])
        (if (equal? k k1)
            (cons
              (cons k v)
              tail)
            (cons
              head
              (put k v tail))))))

(define any? ormap)

(define (put k v al)
  (let ([keys (map car al)])
    (if (any? (lambda (key) (equal? k key)) keys)
        (map
          (lambda (kvp)
            (if (equal? k (car kvp))
                (cons k v)
                kvp))
          al)
        (cons (cons k v) al))))


; (let ([tests `((('((1 2 3)
;                    (4 5 6)))
;                 . '(1 5))
;                (('((1 2)
;                    (3 4)
;                    (5 6)))
;                 . '(1 4)))])
;   (gen-test-suite main-diagonal tests))

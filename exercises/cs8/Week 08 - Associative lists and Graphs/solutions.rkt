#lang racket

(define (filter p? l)
  (cond ((null? l) l)
        ((p? (car l)) (cons (car l) (filter p? (cdr l))))
        (else (filter p? (cdr l)))))

(define (foldr op nv l)
  (if (null? l) nv
      (op (car l) (foldr op nv (cdr l)))))

(define (foldl op nv l)
  (if (null? l) nv
      (foldl op (op nv (car l)) (cdr l))))

(define (member? el lst)
  (cond [(null? lst) #f]
        [(equal? (car lst) el) #t]
        [else (member? el (cdr lst))]))

(define (make-assoc f l)
  (map (lambda (x)
         (cons x (f x))) l))

(define assoc-list (make-assoc (lambda (x) (* x x)) '(1 2 3 4 5)))

(define graph '((1 2 4)
                (2 3 5)
                (3 1 5)
                (4)
                (5 2 4)))

(define (keys al)
  (map car al))

(define (values al)
  (map cdr al))

(define (search key al)
  (foldl (lambda (res curr)
           (if (equal? key (car curr)) curr res))
         #f
         al))

(define (add-assoc key value al)
  (let ([pair (search key al)])
    (if (not pair)
        (cons (cons key value) al)
        (map (lambda (x)
               (if (equal? x pair)
                   (cons key value)
                   x)) al))))

(define (histogram lst)
  (foldr (lambda (curr res)
           (let ([pair (search curr res)])
             (if pair
                 (add-assoc curr (+ 1 (cdr pair)) res)
                 (add-assoc curr 1 res))))
         '()
         lst))

(define vertices keys)

(define (children u g)
  (cdr (search u g)))

(define (edge? u v g)
  (member? v (children u g)))

(define (add-edge u v g)
  (add-assoc u (cons v (children u g)) g))

(define (parents u g)
  (filter (lambda (x)
            (member? u (children x g)))
          (vertices g)))

(define (degrees g)
  (map (lambda (x)
         (cons (length (parents x g))
               (length (children x g))))
       (vertices g)))

(define (invert g)
  (map (lambda (x)
         (cons x (parents x g)))
       (vertices g)))

(define (dfs g)
  (define (dfs-helper path)
    (foldl (lambda (res curr)
             (if (member? curr res)
                 res
                 (dfs-helper (cons curr res))))
           path
           (children (car path) g)))
  (reverse (dfs-helper (list (caar g)))))

(define (path u v g)
  (define (dfs-path path)
    (cond [(equal? v (car path))
           (reverse path)]
          [(member? (car path)
                    (cdr path)) #f]
          [else (foldl (lambda (res curr)
                         (or res (dfs-path (cons curr path))))
                         #f (children (car path) g))]))
  (dfs-path (list u)))
        
(define (bfs g)
  (define (next u visited)
    (filter (lambda (x)
              (not (member? x visited)))
            (children u g)))
  (define (helper level visited)
    (if (null? level) visited
        (let ([newLevel (foldl (lambda (res curr)
                                 (let ((n (reverse (next curr (cdr res)))))
                                   (cons (append n (car res))
                                         (append n (cdr res)))))
                               (cons '() visited)
                               level)])
          (helper (car newLevel)
                  (cdr newLevel)))))
  (reverse (helper (list (caar g))
                   (list (caar g)))))















  
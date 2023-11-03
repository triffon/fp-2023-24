(define (foldr operation null_value lst)
  (if (null? lst) null_value
      (operation (car lst)
          (foldr operation null_value (cdr lst)))))

(define (foldl operation null_value lst)
  (if (null? lst) null_value
      (foldl operation (operation null_value (car lst)) (cdr lst))))

(define (foldr1 operation lst)
  (foldr operation (car lst) (cdr lst)))

(define (sum lst)
  (foldr1 + lst))

(define (max-element lst)
  (foldr1 max lst))

(define (filter* pred? lst)
  (foldr (lambda (curr res)
           (if (pred? curr)
               (cons curr res)
               res))
         '()
         lst))

(define (map* f lst)
  (foldr (lambda (curr res)
           (cons (f curr) res))
         '()
         lst))

(define (any? pred? lst)
  (foldl (lambda (res curr)
           (or (pred? curr) res))
         #f
         lst))

(define (member? el lst)
  (any? (lambda (x)
          (equal? el x)) lst))

(define (unique lst)
  (foldr (lambda (curr res)
           (if (member? curr res)
               res
               (cons curr res)))
         '()
         lst))

(define (union lst1 lst2)
  (unique (append lst1 lst2)))

(define (intersection lst1 lst2)
  (filter* (lambda (x)
            (member? x lst2))
          lst1))

(define (flip f)
  (lambda (x y)
    (f y x)))

(define (reverse* lst)
  (foldl (flip cons) '() lst))

(define (quick-sort lst)
  (if (null? lst) lst
      (append (quick-sort
               (filter* (lambda (x)
                          (< x (car lst)))
                        (cdr lst)))
              (cons (car lst)
                    (quick-sort
                     (filter* (lambda (x)
                                (>= x (car lst)))
                              (cdr lst)))))))
(define (max-element lst)
  (apply max lst))

(define (average x . args)
  (let ((lst (cons x args)))
    (/ (apply + lst)
       (length lst))))

(define (map-var f . args)
  (if (any? null? args) '()
      (cons (apply f (map car args))
            (apply map-var f (map cdr args)))))








   
(define (foldr op nv l)
  (if (null? l) nv
      (op (car l) (foldr op nv (cdr l)))))

(define (filter pred? l)
  (foldr (lambda (curr res)
           (if (pred? curr)
               (cons curr res)
               res)) '() l))

(define info '(("Georgi" 21 176 66)
               ("Ivan" 25 181 78)
               ("Pesho" 18 186 85)
               ("Alex" 37 174 97)
               ("Dragan" 15 168 72)
               ("Pavel" 14 177 76)))

(define name car)
(define age cadr)
(define height caddr)
(define weight cadddr)

(define (sortBy f lst)
  (if (null? lst) '()
      (append (sortBy f (filter (lambda (x)
                              (< (f x)
                                 (f (car lst))))
                            (cdr lst)))
              (list (car lst))
              (sortBy f (filter (lambda (x)
                              (>= (f x)
                                  (f (car lst))))
                            (cdr lst))))))

(define (optimal-ratio? person)
  (< (/ (weight person)
        (height person))
     0.45))

(define (solution people)
  (map name (sortBy age (filter optimal-ratio? people))))

(solution info) ; 
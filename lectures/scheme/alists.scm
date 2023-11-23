(define (foldr op nv l)
  (if (null? l) nv
      (op (car l) (foldr op nv (cdr l)))))

(define (foldl op nv l)
  (if (null? l) nv
      (foldl op (op nv (car l)) (cdr l))))

(define (filter p? l)
  (foldr (lambda (x r) (if (p? x) (cons x r) r)) '() l))

(define (all? p? l)
  (foldr (lambda (x r) (and (p? x) r)) #t l))


(define (make-alist f keys)
  (map (lambda (key) (cons key (f key))) keys))

(define (keys al)
  (map car al))

(define (values al)
  (map cdr al))

(define (alist? x)
  (and (list? x) (all? pair? x)))

(define (associate equality? key al)
  (cond ((null? al) #f)
        ((equality? key (caar al)) (car al))
        (else (associate equality? key (cdr al)))))

(define (assoc key al)
  (associate equal? key al))

(define al (make-alist (lambda (x) (* x x)) '(1 2 3 4 5)))

(define (del-assoc key al)
  (foldr (lambda (kv r) (if (equal? (car kv) key) r (cons kv r))) '() al))

(define (del-assoc key al)
  (filter (lambda (kv) (not (equal? (car kv) key))) al))

(define (set-assoc key value al)
  (cons (cons key value) (del-assoc key al)))
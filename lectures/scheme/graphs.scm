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

(define (exists? p? l)
  (not (null? (filter p? l))))

;;(define (exists? p? l)
;;  (and (not (null? l)) (or (p? (car l)) (exists? p? (cdr l)))))

(define (exists p? l)
  (and (not (null? l)) (or (and (p? (car l)) (car l)) (exists p? (cdr l)))))

(define (search p l)
  (and (not (null? l)) (or (p (car l)) (search p (cdr l)))))

(define (all? p? l)
  (not (search (lambda (x) (not (p? x))) l)))

(define g '((1 2 3) (2 3) (3 4 5) (4) (5 2 4 6) (6 2)))

(define (vertices g) (map car g))

(define (children u g) (cdr (assq u g)))

(define (edge? u v g) (memv v (children u g)))

(define (map-children v f g)
  (map f (children v g)))

(define (search-child v f g)
  (search f (children v g)))

(define (childless g)
  (filter (lambda (v) (null? (children v g))) (vertices g)))

(define (parents u g)
  (filter (lambda (v) (edge? v u g)) (vertices g)))

(define (symmetric? g)
  (all? (lambda (u) (all? (lambda (v) (edge? v u g)) (children u g))) (vertices g)))

(define (cons#f h t)
  (and t (cons h t)))

(define (dfs-path u v g)
  (if (eqv? u v) (list u)
      (search-child u (lambda (w) (cons#f u (dfs-path w v g))) g)))

(define (dfs-path u v g)
  (define (dfs-search path)
    (let ((current-u (car path)))
      (cond ((eqv? current-u v) (reverse path)) ;; лесен хубав вариант
            ((memv current-u (cdr path)) #f)    ;; лесен лош вариант
            (else (search-child current-u
                                (lambda (w) (dfs-search (cons w path))) g)))))
  (dfs-search (list u)))

(define (extend path g)
  (map-children (car path) (lambda (u) (cons u path)) g))

;; Path -> Level
;; Path -> [Path]
;; [Vertex] -> [[Vertex]]
(define (extend-acyclic path g)
  (define (remains-acyclic? path)
    (not (memv (car path) (cdr path))))
  (filter remains-acyclic? (extend path g)))

;; [Path] -> [Level]
;; [Path] -> [[Path]]
;; [[Vertex]] -> [[[Vertex]]]

;; искаме Level -> Level
;; [Path] -> [Path]
(define (extend-level level g)
  (apply append (map (lambda (path) (extend-acyclic path g)) level)))

(define (bfs-path u v g)
  (define (extend path)
    (map-children (car path) (lambda (u) (cons u path)) g))
  (define (remains-acyclic? path)
    (not (memv (car path) (cdr path))))
  (define (extend-acyclic path)
    (filter remains-acyclic? (extend path)))
  (define (extend-level level)
    (apply append (map extend-acyclic level)))
  (define (target-path path)
    (and (eqv? (car path) v) (reverse path)))
  (define (bfs-level level)
    (and (not (null? level))
         (or (search target-path level)
             (bfs-level (extend-level level)))))
  (bfs-level (list (list u))))
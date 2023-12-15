#lang racket

(define t1 '(94 (73 () (80 () ())) (87 (73 () ()) ())))
(define t2 '(94 (73 () (80 () ())) (87 (73 () ()) (7 () ()))))
(define t3 '(94 (0 () (42 () ())) (0 (42 () ()) (42 () ()))))
(define t4 '(20 (15 () (10 () ())) (15 (10 () ()) (10 () ()))))

(define (root bt) (car bt))
(define (left-subtree bt) (cadr bt))
(define (right-subtree bt) (caddr bt))

(define (at-level level bt)
  (cond
    [(empty? bt) '()]
    [(zero? level) (list (root bt))]
    [else (append (at-level (sub1 level) (left-subtree bt)) (at-level (sub1 level) (right-subtree bt)))]
    )
  )

(define (maximum-level bt)
  (define (helper current-level max-sum max-level)
    (let* (
           [current-level-elements (at-level current-level bt)]
           [current-sum (apply + current-level-elements)]
           )
      (cond
        [(empty? current-level-elements) max-level]
        [(>= current-sum max-sum) (helper (add1 current-level) current-sum current-level)]
        [else (helper (add1 current-level) max-sum max-level)]
        )
      )
    )
  (if (empty? bt) -1 (helper 1 (root bt) 0))
  )

(= (maximum-level t1) 1)
(= (maximum-level t2) 2)
(= (maximum-level t3) 2)
(= (maximum-level t4) 2)
(= (maximum-level '()) -1)
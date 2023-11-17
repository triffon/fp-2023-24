#lang racket

(define (foldr operation null_value lst)
  (if (null? lst) null_value
      (operation (car lst)
          (foldr operation null_value (cdr lst)))))
(define (foldl operation null_value lst)
  (if (null? lst) null_value
      (foldl operation (operation null_value (car lst)) (cdr lst))))
(define (make-tree root left right) (list root left right))
(define (make-leaf root) (list root '() '()))
(define root car)
(define left cadr)
(define right caddr)
(define empty? null?)

(define t (make-tree 5
                     (make-tree 1
                                (make-tree 4
                                           '()
                                           (make-leaf 13))
                                (make-leaf 3))
                     (make-tree 8
                                (make-tree 0
                                           (make-leaf 10)
                                           (make-leaf 9))
                                (make-leaf 11))))

(define bst (make-tree 3
                       (make-tree 1
                                  '()
                                  (make-leaf 2))
                       (make-tree 4
                                  '()
                                  (make-leaf 5))))

(define derivation-tree (make-tree +
                                   (make-tree -
                                              (make-tree +
                                                         (make-leaf 5)
                                                         (make-leaf 7))
                                              (make-leaf 3))
                                   (make-tree *
                                              (make-tree +
                                                         (make-tree /
                                                                    (make-leaf 20)
                                                                    (make-leaf 4))
                                                         (make-leaf 9))
                                              (make-leaf 6))))
(define mirror (make-tree 4
                          (make-tree 1
                                     (make-tree 7
                                                '()
                                                (make-leaf 10))
                                     (make-leaf 2))
                          (make-tree 6
                                     (make-leaf 0)
                                     (make-tree 8
                                                (make-leaf 11)
                                                '()))))

(define (leaf? t)
  (and (not (empty? t))
       (empty? (left t))
       (empty? (right t))))

(define (tree-to-list t)
  (cond [(empty? t) '()]
        [(leaf? t) (list (root t))]
        [else (append (tree-to-list (left t))
                      (list (root t))
                      (tree-to-list (right t)))]))

(define (bst-insert el t)
  (cond [(empty? t)
         (make-leaf el)]
        [(> el (root t))
         (make-tree (root t)
                    (left t)
                    (bst-insert el (right t)))]
        [else (make-tree (root t)
                         (bst-insert el (left t))
                         (right t))]))

(define (tree-sort lst)
  (tree-to-list (foldr bst-insert '() lst)))

(define (sorted? lst)
  (cdr (foldl (lambda (res curr)
            (cons curr (and (cdr res)
                            (<= (car res) curr))))
         (cons (car lst) #t)
         (cdr lst))))

(define (bst? tree)
  (sorted? (tree-to-list tree)))

(define (calculate tree)
  (if (leaf? tree)
      (root tree)
      ((root tree)
       (calculate (left tree))
       (calculate (right tree)))))

(define (inverse tree)
  (if (empty? tree) '()
      (make-tree (- (root tree))
                 (inverse (right tree))
                 (inverse (left tree)))))

(define (mirror? tree)
  (define (iff-empty? tree1 tree2)
    (or (and (empty? tree1)
             (empty? tree2))
        (and (not (empty? tree1))
             (not (empty? tree2)))))
  (define (symetric? tree1 tree2)
    (cond [(and (empty? tree1)
             (empty? tree2)) #t]
          [(not (and (iff-empty? (left tree1)
                                 (right tree2))
                     (iff-empty? (right tree1)
                                 (left tree2)))) #f]
          [else (and (symetric? (left tree1)
                                (right tree2))
                     (symetric? (right tree1)
                                (left tree2)))]))
  (symetric? (left tree)
             (right tree)))


(define (bst-remove el tree)
  (define (find-next tree)
    (if (empty? (left tree))
        (root tree)
        (find-next (left tree))))
  (define (remove-root tree)
    (if (empty? (right tree))
        (left tree)
        (let ((next (find-next (right tree))))
          (make-tree next
                     (left tree)
                     (bst-remove next (right tree))))))
  (cond [(empty? tree) '()]
        [(equal? (root tree) el) (remove-root tree)]
        [(< el (root tree))
         (bst-remove el (left tree))]
        [else (bst-remove` el (right tree))]))
  

    










   



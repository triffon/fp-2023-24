;; помощни функции

(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (accumulate-i op nv a b term next)
  (if (> a b) nv
      (accumulate-i op (op nv (term a)) (next a) b term next)))

(define error car)

(define (id x) x)
(define (1+ x) (+ x 1))

(define (fact n)
  (accumulate * 1 1 n (lambda (i) i) 1+))

(define (pow x n)
  (accumulate * 1 1 n (lambda (i) x) 1+))

;; ниво 0 - представяне
;; [DEPRECATED] наредена двойка от цели числа, като второто е ненулево
;; [DEPRECATED] наредена двойка от взаимно прости цели числа, като второто е ненулево
;; [DEPRECATED] наредена двойка от взаимно прости цели числа, като второто е положително
;; [DEPRECATED] наредена двойка от етикет 'rat и наредена двойка от взаимно прости цели числа, като второто е положително
;; анонимна функция, която сочи към среда, съдържаща числителя и знаменателя
;; и получава като параметър метод, който да извика над данните
(define (rat? p)
  (and (pair? p)
       (eqv? (car p) 'rat)
       (let ((r (cdr p)))
         (and (pair? r)
              (integer? (car r))
              (integer? (cdr r))
              (positive? (cdr r))
              (= (gcd (car r) (cdr r)) 1)))))
  
;; ниво 1 - базови операции
(define make-rat cons)
(define get-numer car)
(define get-denom cdr)

(define (make-rat n d)
  (if (= d 0) (error "make-rat е извикан с нулев знаменател")
      (cons n d)))

(define (make-rat n d)
  (if (= d 0) (error "make-rat е извикан с нулев знаменател")
      (let ((g (gcd n d)))
        (cons (/ n g) (/ d g)))))

(define (make-rat n d)
  (cons 'rat
        (if (= d 0) (error "make-rat е извикан с нулев знаменател")
            (let ((g (gcd n d)))
              (if (< d 0)
                  (cons (/ (- n) g) (/ (- d) g))
                  (cons (/ n g) (/ d g)))))))

(define (make-rat n d)
  (if (= d 0) (error "make-rat е извикан с нулев знаменател")
      (let* ((g (gcd n d))
             (sign (if (< d 0) -1 1))
             (numer (* sign (quotient n g)))
             (denom (* sign (quotient d g))))
        ;; (cons numer denom)
        (define (self prop . params)
          (case prop
            ('get-numer numer)
            ('get-denom denom)
            ('print (cons numer denom))
            ('* (let ((other (car params)))
                  (make-rat (* (self 'get-numer) (other 'get-numer))
                            (* (self 'get-denom) (other 'get-denom)))))
            ('type 'rat)
            (else (error "непознат метод на rat"))))
        self)))

(define (rat? r)
  (eqv? (r 'type) 'rat))

(define get-numer cadr)
(define get-denom cddr)

(define (check-rat f)
  (lambda (p)
    (if (rat? p) (f p) (error "Очаква се рационално число"))))

(define get-numer (check-rat cadr))
(define get-denom (check-rat cddr))

(define (get-numer r) (r 'get-numer))
(define (get-denom r) (r 'get-denom))

;; ниво 2 - аритметични операции
(define (*-rat p q)
  (make-rat (* (get-numer p) (get-numer q))
            (* (get-denom p) (get-denom q))))

(define (+-rat p q)
  (make-rat (+ (* (get-numer p) (get-denom q)) (* (get-numer q) (get-denom p)))
             (* (get-denom p) (get-denom q))))

(define (<-rat p q)
  (< (* (get-numer p) (get-denom q)) (* (get-numer q) (get-denom p))))

(define 0-rat (make-rat 0 1))

;; ниво 3 - приложни програми
(define (my-exp x n)
  (accumulate +-rat 0-rat 0 n (lambda (i) (make-rat (pow x i) (fact i))) 1+))

(define (to-double r)
  (+ .0 (/ (get-numer r) (get-denom r))))
#lang racket

;; Utils

(define all? andmap)
(define any? ormap)

;;;;;;;;;;;;;;
;;; Graphs ;;;
;;;;;;;;;;;;;;

;; {{{ graph?

(define (graph? g)
  ;; {{{
  ;; (and (list? g)
  ;;      (all? list? g)
  ;; }}}
  (and (list? g)
       (all? pair? g)
       (all? (lambda (pair)
               (list? (cdr pair))))))

;; }}}

;; (out-deg g x), която връща броя излизащи ребра от x

(define (out-deg g x)
  (length (cdr (assoc x g))))

;; (in-deg g x), която връща броя влизащи ребра в x

(define (in-deg g x)
  (count
    (lambda (pair)
      (member x (cdr pair)))
    g))

;;;;;;;;;;;;;;;
;;; Streams ;;;
;;;;;;;;;;;;;;;

;; {{{ my-delay

(define-syntax my-delay
  (syntax-rules ()
    ((_ nqkyv-kod)
     (lambda ()
       nqkyv-kod))))

;; }}}

;; {{{ my-force

(define my-force
  (lambda (promise)
    (promise)))

;; }}}

;; {{{ my-stream-cons

(define-syntax my-stream-cons
  (syntax-rules ()
    ((_ fst rst)
     (cons fst (my-delay rst)))))

;; }}}

;; {{{ (my-stream-first s)

(define (my-stream-first s)
  (car s))

;; }}}

;; {{{ (my-stream-rest s)

(define (my-stream-rest s)
  (my-force (cdr s)))

;; }}}

;; {{{ (nats-helper n)

(define (nats-from n)
  (my-stream-cons
   n
   (nats-from (+ n 1))))

;; }}}

;; {{{ nats

(define nats
  (nats-from 0))

;; }}}

;; {{{ (stream-take s n)

(define (stream-take s n)
  (if (= n 0)
      '()
      (cons (my-stream-first s)
            (stream-take (my-stream-rest s)
                         (- n 1)))))

;; }}}

;; vim: set foldmethod=marker:

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

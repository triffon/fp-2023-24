#lang racket

;; Testing, don't look (yet :D)
(require rackunit)
(require rackunit/text-ui)

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

(define-syntax (gen-test-suite stx)
  (syntax-case stx ()
    [(_ fn tests)
     (with-syntax ([header (format "~a" (syntax->datum #'fn))])
       #'(begin
           (display
            (string-append "\nTesting " header ":\n"))

           (time
            (eval
             `(run-tests
               (test-suite
                ,header
                ,@(map (lambda (test-params)
                         (let ([args     (car test-params)]
                               [expected (cdr test-params)])
                           `((if (procedure? ,expected)
                                 check-pred
                                 check-equal?)
                              ,expected
                              (,fn ,@args))))
                       tests)))
             ns))
           (values)))]))

(provide (all-defined-out))
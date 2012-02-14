#lang racket
(require rackunit
         "../passes.rkt")
(require rackunit/text-ui)

(define explicit-defined-lambdas-tests
  (test-suite
   "Tests that defined functions are expanded to explicit lambdas"
   
   (test-case
    "Implicit lambda to explicit"
    (let ([in-form (datum->syntax #f '(define (some-fun a b c)
                                        (+ a (+ b c))))]
          [out-form '(define some-fun
                       (lambda (a b c)
                         (+ a (+ b c))))])
      (check-equal? (syn-walk in-form explicit-defined-lambdas)
                    out-form)))
   
   (test-case
    "Explicit lambda untouched"
    (let ([in-form (datum->syntax #f '(define some-fun
                                        (lambda (a b c)
                                          (+ a (+ b c)))))]
          [out-form '(define some-fun
                       (lambda (a b c)
                         (+ a (+ b c))))])
      (check-equal? (syn-walk in-form explicit-defined-lambdas)
                    out-form)))
    ))

(run-tests explicit-defined-lambdas-tests)
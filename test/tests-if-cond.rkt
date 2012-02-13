#lang racket
(require rackunit
         "../passes.rkt")
(require rackunit/text-ui)

(define if-cond-tests
  (test-suite
   "Tests of if and cond forms"
   
   (test-case
    "Single argument if"
    (let ([in-form (datum->syntax #f '(if condition 'then-expr))]
          [out-form '(if condition 'then-expr #f)])
      (check-equal? (syn-walk in-form if-expand)
                    out-form)))
   
   (test-case
    "cond to if"
    (let ([in-form (datum->syntax #f '(cond [a-cond
                                             'then-a-one
                                             'then-a-two]
                                            [b-cond
                                             'then-b-one]))]
          [out-form '(if a-cond
                         (begin 'then-a-one
                                'then-a-two)
                         (if b-cond
                             (begin 'then-b-one)
                             #f))])
      (check-equal? (syn-walk in-form if-expand)
                    out-form)))

   (test-case
    "cond to if without else"
    (let ([in-form (datum->syntax #f '(cond [a-cond
                                             'then-a-one
                                             'then-a-two]
                                            [b-cond
                                             'then-b-one]
                                            [else
                                             'then-else]))]
          [out-form '(if a-cond
                         (begin 'then-a-one
                                'then-a-two)
                         (if b-cond
                             (begin 'then-b-one)
                             (begin 'then-else)))])
      (check-equal? (syn-walk in-form if-expand)
                    out-form)))
    
   (test-case
    "cond to if with else"
    (let ([in-form (datum->syntax #f '(cond [a-cond
                                             'then-a-one
                                             'then-a-two]
                                            [b-cond
                                             'then-b-one]))]
          [out-form '(if a-cond
                         (begin 'then-a-one
                                'then-a-two)
                         (if b-cond
                             (begin 'then-b-one)
                             #f))])
      (check-equal? (syn-walk in-form if-expand)
                    out-form))
    )))

(run-tests if-cond-tests)
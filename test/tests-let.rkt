#lang racket
(require rackunit
         "../passes.rkt")
(require rackunit/text-ui)

(define let-tests
  (test-suite
   "Tests of let form"
   
   ; Single variable let
   (let ([in-form  (datum->syntax #f '(let ([x 5]) (+ 2 x)))]
         [out-form '((lambda (x) (+ 2 x)) 5)])
     (check-equal? (syn-walk in-form let-expand)
                   out-form))
   
   ; Multi-variate let
   (let ([in-form  (datum->syntax #f '(let ([x 5] [y 2]) (+ 2 x y)))]
         [out-form '((lambda (x y) (+ 2 x y)) 5 2)])
     (check-equal? (syn-walk in-form let-expand)
                   out-form))
   
   ; Nested let
   (let ([in-form  (datum->syntax #f '(let ([x 5]) (let ([y 2]) (- (+ 2 x) y))))]
         [out-form '((lambda (x)
                       ((lambda (y)
                          (- (+ 2 x) y))
                        2))
                     5)])
     (check-equal? (syn-walk in-form let-expand)
                   out-form))
   
   ; Let that's not in leading position
   (let ([in-form  (datum->syntax #f '(cons 'a (let ([x 5]) (+ 2 x))))]
         [out-form '(cons 'a ((lambda (x) (+ 2 x)) 5))])
     (check-equal? (syn-walk in-form let-expand)
                   out-form))
   ))

(run-tests let-tests)
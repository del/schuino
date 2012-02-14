#lang racket
(require rackunit
         "../passes.rkt")
(require rackunit/text-ui)

(define closure-conversion-tests
  (test-suite
   "Tests of closure conversion"
   
   (test-case
    "Free variables"
    (check-equal? (free '(make-closure (lambda (env x) ((env-ref env f) x))
                                       (make-env (f f))))
                  (set 'f)))
   
   (test-case
    "Substitution"
    (check-equal? 
     (substitute (make-hash '((f . +) (x . 4)))
                 '(make-closure (lambda (env x) ((env-ref env f) x))
                                (make-env (f f))))
     '(make-closure (lambda (env x) ((env-ref env f) x))
                    (make-env (f +)))))
    ))

(run-tests closure-conversion-tests)
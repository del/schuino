#lang racket
(provide syn-walk
         let-expand
         if-expand
         cond-expand
         explicit-defined-lambdas
         free
         substitute
         closure-convert
         transform/bottom-up
         transform/top-down)

; Helper functions for walking the syntax tree and
; applying a transformation pass to it
(define (syn-walk syn pass)
  (let ([datum (maybe-syntax->datum syn)])
    (cond
      [(pair? datum)
       (maybe-syntax->datum
        (pass (maybe-syntax->datum (cons (syn-walk (first datum) pass)
                                         (syn-walk (rest datum) pass)))))]
      [else (maybe-syntax->datum (pass datum))])))

(define (maybe-syntax->datum syn)
  (if (syntax? syn)
      (syntax->datum syn)
      syn))

;;;=============================================================================
;;; Compiler passes
;;;=============================================================================

;;;-----------------------------------------------------------------------------
;;; Let expand pass
;;;-----------------------------------------------------------------------------
(define (let-expand exp)
  (syntax-case exp ()
    [(let ((i v) ...) e1 e2 ...)
     (syntax ((lambda (i ...) e1 e2 ...) v ...))]

    [exp (syntax exp)]))

;;;-----------------------------------------------------------------------------
;;; If and cond expansion pass
;;;-----------------------------------------------------------------------------
(define (if-expand exp)
    (match exp
      [(cons 'cond cond-clauses)
       (cond-expand cond-clauses)]

      [(list 'if test-expr then-expr)
       (list 'if test-expr then-expr #f)]

      [other-exp other-exp]))

(define (cond-expand clauses)
  (if (empty? clauses)
      #f
      (let* ([first-clause (first clauses)]
             [first-test-expr (first first-clause)]
             [first-then-exprs (rest first-clause)]
             [rest-clauses (rest clauses)])
        (if (equal? first-test-expr 'else)
            `(begin ,@first-then-exprs)
            `(if ,first-test-expr
                 (begin ,@first-then-exprs)
                 ,(cond-expand rest-clauses))))))

(define (maybe-syntax->datum syn)
  (if (syntax? syn)
      (syntax->datum syn)
      syn))
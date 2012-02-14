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

;;;-----------------------------------------------------------------------------
;;; Pass to make lambdas in define forms explicit
;;;-----------------------------------------------------------------------------
(define (explicit-defined-lambdas exp)
  (match exp
    [`(define (,name ,params ...) ,body)
     `(define ,name (lambda ,params ,body))]
    [_ exp]))

;;;-----------------------------------------------------------------------------
;;; Closure conversion pass
;;;-----------------------------------------------------------------------------

;; Borrowed from Matt Might,
;; http://matt.might.net/articles/closure-conversion/code/closure-convert.rkt

;; <exp> ::= (lambda (<var> ...) <exp>)
;;        |  (<exp> <exp> ...)
;;        |  <var>

;;        ;  added for closure conversion:
;;        |  (lambda* (<var> <var> ...) <exp>)
;;        |  (make-closure <exp> <exp>)
;;        |  (make-env (<var> <exp>) ...)
;;        |  (env-ref <exp> <var>)
;;        |  (apply-closure <exp> <exp> ...)

(require test-engine/racket-tests)

; free : exp => set[var]
(define (free exp)
  (match exp
    [`(lambda ,params ,body)
     (set-subtract (free body) (apply set params))]

    [`(lambda* ,params ,body)
     (set-subtract (free body) (apply set params))]

    [(? symbol?)
     (set exp)]

    [`(make-closure ,proc, env)
     (set-union (free proc) (free env))]

    [`(make-env (,vs ,es) ...)
     (apply set-union (map free es))]

    [`(env-ref ,env ,v)
     (free env)]

    [`(apply-closure ,f ,args ...)
     (apply set-union (map free `(,f . ,args)))]

    [`(,f ,args ...)
     (apply set-union (map free `(,f . ,args)))]))


; substitute : hash[var,exp] exp => exp
(define (substitute sub exp)
  (match exp
    [`(lambda ,params ,body)
      (define params* (apply set params))
      (define sub*
        (for/hash ([(k v) sub] #:when (not (set-member? params* k)))
          (values k v)))
      `(lambda ,params ,(substitute sub* body))]

    [`(lambda* ,params ,body)
     ; should not have free variables
      (define params* (apply set params))
      (define sub*
        (for/hash ([(k v) sub] #:when (not (set-member? params* k)))
          (values k v)))
      `(lambda* ,params ,(substitute sub* body))]

    [(? symbol?)
     (if (hash-has-key? sub exp)
         (hash-ref sub exp)
         exp)]

    [`(make-closure ,lam ,env)
     `(make-closure ,(substitute sub lam) ,(substitute sub env))]

    [`(make-env (,vs ,es) ...)
     `(make-env ,@(map list vs (map (substitute-with sub) es)))]

    [`(env-ref ,env ,v)
     `(env-ref ,(substitute sub env) ,v)]

    [`(apply-closure ,f ,args ...)
     `(apply-closure ,@(map (substitute-with sub) `(,f . ,args)))]

    [`(,f ,args ...)
     (map (substitute-with sub) `(,f . ,args))]))

(define (substitute-with sub)
  (lambda (exp)
    (substitute sub exp)))


(define (closure-convert exp)
  (match exp
    [`(lambda ,params ,body)
     (define $env (gensym 'env))
     (define params* (cons $env params))
     (define fv (free exp))
     (define env (for/list ((v fv))
                   (list v v)))
     (define sub (for/hash ((v fv))
                   (values v `(env-ref ,$env ,v))))
     (define body* (substitute sub body))
     `(make-closure (lambda* ,params* ,body*)
                    (make-env ,@env))]

    [`(lambda* ,params ,body)
     exp]

    [(? symbol?)
     exp]

    [`(make-closure ,lam ,env)
     exp]

    [`(make-env (,vs ,es) ...)
     exp]

    [`(env-ref ,env ,v)
     exp]

    [`(apply-closure ,f ,args ...)
     exp]

    [`(,f ,args ...)
     `(apply-closure ,f . ,args)]))


; transform/bottom-up : applies a bottom-up tree transform
(define (transform/bottom-up f exp)

  (define (t e) (transform/bottom-up f e))

  (let ([exp* (match exp
                [`(lambda ,params ,body)
                 `(lambda ,params ,(t body))]

                [`(lambda* ,params ,body)
                 `(lambda* ,params ,(t body))]

                [(? symbol?)
                 exp]

                [`(make-closure ,lam ,env)
                 `(make-closure ,(t lam) ,(t env))]

                [`(make-env (,vs ,es) ...)
                 `(make-env ,@(map list vs (map t es)))]

                [`(env-ref ,env ,v)
                 `(env-ref ,(t env) ,v)]

                [`(apply-closure ,f ,args ...)
                 `(apply-closure ,(t f) ,(map t args))]

                [`(,f ,args ...)
                 `(,(t f) ,@(map t args))])])
    (f exp*)))


; transform/top-down : applies a bottom-up tree transform
(define (transform/top-down f exp)

  (define (t e) (transform/top-down f e))

  (match (f exp)
    [`(lambda ,params ,body)
     `(lambda ,params ,(t body))]

    [`(lambda* ,params ,body)
     `(lambda* ,params ,(t body))]

    [(? symbol?)
     exp]

    [`(make-closure ,lam ,env)
     `(make-closure ,(t lam) ,(t env))]

    [`(make-env (,vs ,es) ...)
     `(make-env ,@(map list vs (map t es)))]

    [`(env-ref ,env ,v)
     `(env-ref ,(t env) ,v)]

    [`(apply-closure ,f ,args ...)
     `(apply-closure ,(t f) ,@(map t args))]

    [`(,f ,args ...)
     `(,(t f) ,@(map t args))]))

(define (flat-closure-convert exp)
  (transform/bottom-up closure-convert exp))

(define (shared-closure-convert exp)
  (transform/top-down closure-convert exp))


(define example
  '(lambda (f)
     (lambda (z)
       (lambda (x)
         (f x z a)))))


(pretty-write
 (closure-convert '(lambda (x) (+ x a b))))

(pretty-write (flat-closure-convert example))

(pretty-write (shared-closure-convert example))
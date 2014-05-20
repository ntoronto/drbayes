#lang racket/base

(require (for-syntax racket/base
                     syntax/id-table)
         racket/stxparam
         "../untyped-utils.rkt")

(provide define-dispatcher-parameter
         define-dispatcher-id
         define-dispatcher
         define-parameterized-syntax
         dispatcher-set!
         dispatcher-set-value!
         (for-syntax current-dispatcher-id
                     current-dispatcher-ref))

(begin-for-syntax
  (struct expansion-dispatcher (id table) #:transparent)
  )

(define-syntax (define-dispatcher-parameter stx)
  (syntax-case stx ()
    [(_ name)
     (syntax/loc stx
       (define-syntax-parameter name
         (expansion-dispatcher #f (make-immutable-free-id-table))))]))

(define-for-syntax (current-dispatcher-value dispatcher-param)
  (define (fail)
    (raise-syntax-error 'current-dispatcher "not defined as a dispatcher parameter" dispatcher-param))
  (unless (syntax-local-value dispatcher-param (λ () #f)) (fail))
  (define disp (syntax-parameter-value dispatcher-param))
  (unless (expansion-dispatcher? disp) (fail))
  disp)

(define-for-syntax (current-dispatcher-id dispatcher-param)
  (expansion-dispatcher-id (current-dispatcher-value dispatcher-param)))

(define-for-syntax (current-dispatcher-ref dispatcher-param name)
  (free-id-table-ref
   (expansion-dispatcher-table (current-dispatcher-value dispatcher-param))
   name
   (λ () (raise-syntax-error 'current-dispatcher-ref
                             (format "no transformer for ~a in current dispatcher ~a"
                                     (syntax->datum name)
                                     (syntax->datum dispatcher-param))))))

(define-syntax (define-dispatcher-id stx)
  (syntax-case stx ()
    [(_ name)
     (syntax/loc stx
       (define-syntax (name stx)
         (raise-syntax-error 'name "cannot use dispatcher id as an expression" stx)))]))

(define-syntax (define-dispatcher stx)
  (syntax-case stx ()
    [(_ name id)
     (syntax/loc stx
       (define-for-syntax name (expansion-dispatcher id (make-free-id-table))))]))

(define-syntax (define-parameterized-syntax stx)
  (syntax-case stx ()
    [(_ name dispatcher-param)
     (syntax/loc stx
       (define-syntax (name stx)
         ((current-dispatcher-ref #'dispatcher-param #'name) stx)))]))

(define-syntax (dispatcher-set! stx)
  (syntax-case stx ()
    [(_ dispatcher val name)
     (quasisyntax/loc stx
       (begin-for-syntax
         (free-id-table-set! (expansion-dispatcher-table dispatcher)
                             #'val
                             (syntax-local-value #'name))))]))

(define-syntax (dispatcher-set-value! stx)
  (syntax-case stx ()
    [(_ dispatcher val name)
     (quasisyntax/loc stx
       (begin-for-syntax
         (free-id-table-set! (expansion-dispatcher-table dispatcher)
                             #'val
                             (make-head-form #'name))))]))

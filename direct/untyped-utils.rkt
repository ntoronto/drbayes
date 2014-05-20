#lang racket/base

(require (for-syntax racket/base
                     racket/syntax)
         typed/racket/base)

(provide (all-defined-out))

(define-syntax here #f)

(define-syntax (define-singleton-type stx)
  (syntax-case stx ()
    [(_ T t)
     (with-syntax* ([Type  (format-id #'here "~a" #'T)]
                    [Type?  (format-id #'here "~a?" #'T)]
                    [t?  (format-id #'t "~a?" #'t)])
       (syntax/loc stx
         (begin (struct: Type ()
                  #:transparent
                  #:property prop:custom-print-quotable 'never
                  #:property prop:custom-write
                  (λ (_ port write?) (fprintf port "~a" 't)))
                
                (define-type T Type)
                (define t (Type))
                (define t? Type?))))]
    [(_ T Parent t)
     (with-syntax* ([Type  (format-id #'here "~a" #'T)]
                    [Type?  (format-id #'here "~a?" #'T)]
                    [t?  (format-id #'t "~a?" #'t)])
       (syntax/loc stx
         (begin (struct: Type Parent ()
                  #:transparent
                  #:property prop:custom-print-quotable 'never
                  #:property prop:custom-write
                  (λ (_ port write?) (fprintf port "~a" 't)))
                
                (define-type T Type)
                (define t (Type))
                (define t? Type?))))]))

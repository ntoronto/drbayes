#lang racket/base

(require (for-syntax racket/base
                     racket/syntax)
         racket/stxparam
         "parameterized-expansion.rkt"
         "../arrow.rkt")

(provide (all-defined-out)
         (for-syntax (all-defined-out)))

(define-dispatcher-id proc-dispatcher-id)
(define-dispatcher-id bot*-dispatcher-id)
(define-dispatcher-id pre*-dispatcher-id)
(define-dispatcher-id idx-dispatcher-id)

(define-dispatcher-parameter drbayes-dispatcher)
(define-dispatcher proc-dispatcher #'proc-dispatcher-id)
(define-dispatcher bot*-dispatcher #'bot*-dispatcher-id)
(define-dispatcher pre*-dispatcher #'pre*-dispatcher-id)
(define-dispatcher idx-dispatcher #'idx-dispatcher-id)

(define-syntax (define-drbayes-arrow-comp stx)
  (syntax-case stx ()
    [(_ name)
     (with-syntax ([name/arr  (format-id #'name "~a/arr" #'name)]
                   [name/proc  (format-id #'name "~a/proc" #'name)]
                   [name/bot*  (format-id #'name "~a/bot*" #'name)]
                   [name/pre*  (format-id #'name "~a/pre*" #'name)]
                   [name/idx  (format-id #'name "~a/idx" #'name)])
       (syntax/loc stx
         (begin
           (define-parameterized-syntax name/arr drbayes-dispatcher)
           (dispatcher-set-value! proc-dispatcher name/arr name/proc)
           (dispatcher-set-value! bot*-dispatcher name/arr name/bot*)
           (dispatcher-set-value! pre*-dispatcher name/arr name/pre*)
           (dispatcher-set-value! idx-dispatcher name/arr name/idx))))]))

(define-syntax-rule (define-drbayes-arrow-comps name ...)
  (begin (define-drbayes-arrow-comp name) ...))

(define-drbayes-arrow-comps >>> &&& ifte lazy)
(define-drbayes-arrow-comps fail id restrict const ref)
(define-drbayes-arrow-comps random boolean ifte*)
(define-drbayes-arrow-comps null list apply let)
(define-drbayes-arrow-comps tag? tag untag)
(define-drbayes-arrow-comps real? null? pair? boolean?)
(define-drbayes-arrow-comps scale translate)
(define-drbayes-arrow-comps neg exp log sqrt asin acos)
(define-drbayes-arrow-comps floor ceiling round truncate)
(define-drbayes-arrow-comps cauchy normal)
(define-drbayes-arrow-comps + -)
(define-drbayes-arrow-comps negative? positive? nonpositive? nonnegative?)
(define-drbayes-arrow-comps < > <= >=)
(define-drbayes-arrow-comps abs sqr recip)
(define-drbayes-arrow-comps * /)
(define-drbayes-arrow-comps partial-cos partial-sin)

(define-parameterized-syntax meaning-arr drbayes-dispatcher)
(dispatcher-set-value! proc-dispatcher meaning-arr meaning-proc)
(dispatcher-set-value! bot*-dispatcher meaning-arr meaning-bot*)
(dispatcher-set-value! pre*-dispatcher meaning-arr meaning-pre*)
(dispatcher-set-value! idx-dispatcher meaning-arr meaning-idx)

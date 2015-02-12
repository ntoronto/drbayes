#lang typed/racket/base

(require racket/match
         typed/rackunit
         math/flonum
         "../../private/set.rkt"
         "../../private/arrow/pure-lifts/bijection.rkt"
         "utils.rkt")

(: boundary-function (-> Boolean Flonum Flonum Flonum Flonum (-> Flonum Flonum)))
(define ((boundary-function inc? a-min a-max b-min b-max) a)
  (cond [(and (fl> a a-min) (fl< a a-max))  +nan.0]
        [(or (fl< a a-min) (fl> a a-max))  +nan.0]
        [else
         (define a-min? (fl= a a-min))
         (define a-max? (fl= a a-max))
         (let-values ([(a-min? a-max?)  (if inc? (values a-min? a-max?) (values a-max? a-min?))])
           (cond [a-min?  b-min]
                 [a-max?  b-max]
                 [else  +nan.0]))]))

(: check-boundary (-> Boolean Integer (-> Flonum Flonum) Flonum Flonum Flonum Flonum Any))
(define (check-boundary inc? tol f a-min a-max b-min b-max)
  (define g (boundary-function inc? a-min a-max b-min b-max))
  (define as (generate-domain-values a-min a-max))
  ;(printf "    ~a~n" as)
  (for ([a  (in-list as)])
    (define b* (g a))
    (unless (flnan? b*)
      ;(printf "    ~a~n" a)
      (define-values (b0 b1)
        (let ([b+  (flstep b* tol)])
          (values (min b+ b*) (max b+ b*))))
      (define b (f a))
      (check-true (and (fl<= b0 b) (fl<= b b1))
                  (format "failed border check: ~a ~v should be in [~v,~v]; given ~v"
                          f a b0 b1 b))
      (void))))

(: check-bijection-boundary (->* [bijection Symbol] [Natural] Any))
(define (check-bijection-boundary f name [tol 0])
  (printf "Testing bijection ~a~n" name)
  (match-define (bijection inc? X Y fb/rndd fb/rndu fa/rndd fa/rndu) f)
  (define-values (a-min a-max a-min? a-max?) (real-interval-fields X))
  (define-values (b-min b-max b-min? b-max?) (real-interval-fields Y))
  (printf "  Testing fb/rndd~n")
  (check-boundary inc? (- tol) fb/rndd a-min a-max b-min b-max)
  (printf "  Testing fb/rndu~n")
  (check-boundary inc?    tol  fb/rndu a-min a-max b-min b-max)
  (printf "  Testing fa/rndd~n")
  (check-boundary inc? (- tol) fa/rndd b-min b-max a-min a-max)
  (printf "  Testing fa/rndu~n")
  (check-boundary inc?    tol  fa/rndu b-min b-max a-min a-max)
  (newline))

(check-bijection-boundary bij-neg 'bij-neg)

(check-bijection-boundary bij-exp 'bij-exp)
(check-bijection-boundary bij-log 'bij-log)

(check-bijection-boundary bij-expm1 'bij-expm1)
(check-bijection-boundary bij-log1p 'bij-log1p)

(check-bijection-boundary bij-sqrt 'bij-sqrt)
(check-bijection-boundary bij-pos-sqr 'bij-pos-sqr)
(check-bijection-boundary bij-neg-sqr 'bij-neg-sqr)

(check-bijection-boundary bij-pos-recip 'bij-pos-recip)
(check-bijection-boundary bij-neg-recip 'bij-neg-recip)

(check-bijection-boundary bij-asin 'bij-asin 2)
(check-bijection-boundary bij-mono-sin 'bij-mono-sin 2)

(check-bijection-boundary bij-acos 'bij-acos 2)
(check-bijection-boundary bij-mono-cos 'bij-mono-cos 2)

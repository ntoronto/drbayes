#lang typed/racket/base

(require "../set.rkt"
         "pure-lifts.rkt"
         "prob-arrows.rkt")

(provide (except-out (all-defined-out)
                     make-list make-apply make-let))

;; ===================================================================================================
;; Lists

(define null/bot* (const/bot* null))
(define null/pre* (const/pre* null))
(define null/idx any/idx)

(: make-list (All (Arr) ((Arr Arr -> Arr) Arr -> (Arr * -> Arr))))
(define ((make-list &&& null) . ks)
  (foldr &&& null ks))

(define list/bot* (make-list &&&/bot* null/bot*))
(define list/pre* (make-list &&&/pre* null/pre*))
(define list/idx (make-list &&&/idx any/idx))

;; ===================================================================================================
;; First-order function application

(: make-apply (All (Arr) ((Arr Arr -> Arr) (Arr * -> Arr) -> (Arr (Listof Arr) -> Arr))))
(define ((make-apply >>> list) body args)
  ((list (apply list args)) . >>> . body))

(define apply/bot* (make-apply >>>/bot* list/bot*))
(define apply/pre* (make-apply >>>/pre* list/pre*))
(define apply/idx (make-apply >>>/idx list/idx))

;; ===================================================================================================
;; Binding

(: make-let (All (Arr) ((Arr Arr -> Arr) (Arr Arr -> Arr) (-> Arr) -> (Arr Arr -> Arr))))
(define ((make-let >>> &&& id) expr body)
  ((expr . &&& . (id)) . >>> . body))

(define let/bot* (make-let >>>/bot* &&&/bot* id/bot*))
(define let/pre* (make-let >>>/pre* &&&/pre* id/pre*))
(define let/idx (make-let >>>/idx &&&/idx id/idx))

;; ===================================================================================================
;; Equality lifts

(define (equal?/bot*) (η/bot* (equal?/bot)))
(define (equal?/pre*) (η/pre* (equal?/pre)))
(define (equal?/idx) any/idx)

;; ===================================================================================================
;; Tagged value lifts

(define tag?/bot* (λ: ([tag : Tag]) (η/bot* (tag?/bot tag))))
(define tag?/pre* (λ: ([tag : Tag]) (η/pre* (tag?/pre tag))))
(define tag?/idx (λ: ([tag : Tag]) any/idx))

(define tag/bot* (λ: ([tag : Tag]) (η/bot* (tag/bot tag))))
(define tag/pre* (λ: ([tag : Tag]) (η/pre* (tag/pre tag))))
(define tag/idx (λ: ([tag : Tag]) any/idx))

(define untag/bot* (λ: ([tag : Tag]) (η/bot* (untag/bot tag))))
(define untag/pre* (λ: ([tag : Tag]) (η/pre* (untag/pre tag))))
(define untag/idx (λ: ([tag : Tag]) any/idx))

;; ===================================================================================================
;; Primitive type predicate lifts

(define (real?/bot*) (η/bot* (real?/bot)))
(define (real?/pre*) (η/pre* (real?/pre)))
(define (real?/idx) any/idx)

(define (null?/bot*) (η/bot* (null?/bot)))
(define (null?/pre*) (η/pre* (null?/pre)))
(define (null?/idx) any/idx)

(define (pair?/bot*) (η/bot* (pair?/bot)))
(define (pair?/pre*) (η/pre* (pair?/pre)))
(define (pair?/idx) any/idx)

(define (boolean?/bot*) (η/bot* (boolean?/bot)))
(define (boolean?/pre*) (η/pre* (boolean?/pre)))
(define (boolean?/idx) any/idx)

;; ===================================================================================================
;; Real primitive lifts

(define scale/bot* (λ: ([y : Flonum]) (η/bot* (scale/bot y))))
(define scale/pre* (λ: ([y : Flonum]) (η/pre* (scale/pre y))))
(define scale/idx (λ: ([y : Flonum]) any/idx))

(define translate/bot* (λ: ([y : Flonum]) (η/bot* (translate/bot y))))
(define translate/pre* (λ: ([y : Flonum]) (η/pre* (translate/pre y))))
(define translate/idx (λ: ([y : Flonum]) any/idx))

(define (neg/bot*) (η/bot* (neg/bot)))
(define (neg/pre*) (η/pre* (neg/pre)))
(define (neg/idx) any/idx)

(define (exp/bot*) (η/bot* (exp/bot)))
(define (exp/pre*) (η/pre* (exp/pre)))
(define (exp/idx) any/idx)

(define (log/bot*) (η/bot* (log/bot)))
(define (log/pre*) (η/pre* (log/pre)))
(define (log/idx) any/idx)

(define (expm1/bot*) (η/bot* (expm1/bot)))
(define (expm1/pre*) (η/pre* (expm1/pre)))
(define (expm1/idx) any/idx)

(define (log1p/bot*) (η/bot* (log1p/bot)))
(define (log1p/pre*) (η/pre* (log1p/pre)))
(define (log1p/idx) any/idx)

(define (sqrt/bot*) (η/bot* (sqrt/bot)))
(define (sqrt/pre*) (η/pre* (sqrt/pre)))
(define (sqrt/idx) any/idx)

(define (asin/bot*) (η/bot* (asin/bot)))
(define (asin/pre*) (η/pre* (asin/pre)))
(define (asin/idx) any/idx)

(define (acos/bot*) (η/bot* (acos/bot)))
(define (acos/pre*) (η/pre* (acos/pre)))
(define (acos/idx) any/idx)

(define (floor/bot*) (η/bot* (floor/bot)))
(define (floor/pre*) (η/pre* (floor/pre)))
(define (floor/idx) any/idx)

(define (ceiling/bot*) (η/bot* (ceiling/bot)))
(define (ceiling/pre*) (η/pre* (ceiling/pre)))
(define (ceiling/idx) any/idx)

(define (round/bot*) (η/bot* (round/bot)))
(define (round/pre*) (η/pre* (round/pre)))
(define (round/idx) any/idx)

(define (truncate/bot*) (η/bot* (truncate/bot)))
(define (truncate/pre*) (η/pre* (truncate/pre)))
(define (truncate/idx) any/idx)

(define (normal-inv-cdf/bot*) (η/bot* (normal-inv-cdf/bot)))
(define (normal-inv-cdf/pre*) (η/pre* (normal-inv-cdf/pre)))
(define (normal-inv-cdf/idx) any/idx)

(define (cauchy-inv-cdf/bot*) (η/bot* (cauchy-inv-cdf/bot)))
(define (cauchy-inv-cdf/pre*) (η/pre* (cauchy-inv-cdf/pre)))
(define (cauchy-inv-cdf/idx) any/idx)

(define (uniform-inv-cdf/bot*) (η/bot* (uniform-inv-cdf/bot)))
(define (uniform-inv-cdf/pre*) (η/pre* (uniform-inv-cdf/pre)))
(define (uniform-inv-cdf/idx) any/idx)

(define (abs/bot*) (η/bot* (abs/bot)))
(define (abs/pre*) (η/pre* (abs/pre)))
(define (abs/idx) any/idx)

(define (sqr/bot*) (η/bot* (sqr/bot)))
(define (sqr/pre*) (η/pre* (sqr/pre)))
(define (sqr/idx) any/idx)

(define (recip/bot*) (η/bot* (recip/bot)))
(define (recip/pre*) (η/pre* (recip/pre)))
(define (recip/idx) any/idx)

(define (partial-cos/bot*) (η/bot* (partial-cos/bot)))
(define (partial-cos/pre*) (η/pre* (partial-cos/pre)))
(define (partial-cos/idx) any/idx)

(define (partial-sin/bot*) (η/bot* (partial-sin/bot)))
(define (partial-sin/pre*) (η/pre* (partial-sin/pre)))
(define (partial-sin/idx) any/idx)

(define (+/bot*) (η/bot* (+/bot)))
(define (+/pre*) (η/pre* (+/pre)))
(define (+/idx) any/idx)

(define (-/bot*) (η/bot* (-/bot)))
(define (-/pre*) (η/pre* (-/pre)))
(define (-/idx) any/idx)

(define (*/bot*) (η/bot* (*/bot)))
(define (*/pre*) (η/pre* (*/pre)))
(define (*/idx) any/idx)

(define (//bot*) (η/bot* (//bot)))
(define (//pre*) (η/pre* (//pre)))
(define (//idx) any/idx)

;; ===================================================================================================
;; Real predicate lifts

(define (zero?/bot*) (η/bot* (zero?/bot)))
(define (zero?/pre*) (η/pre* (zero?/pre)))
(define (zero?/idx) any/idx)

(define (negative?/bot*) (η/bot* (negative?/bot)))
(define (negative?/pre*) (η/pre* (negative?/pre)))
(define (negative?/idx) any/idx)

(define (positive?/bot*) (η/bot* (positive?/bot)))
(define (positive?/pre*) (η/pre* (positive?/pre)))
(define (positive?/idx) any/idx)

(define (nonpositive?/bot*) (η/bot* (nonpositive?/bot)))
(define (nonpositive?/pre*) (η/pre* (nonpositive?/pre)))
(define (nonpositive?/idx) any/idx)

(define (nonnegative?/bot*) (η/bot* (nonnegative?/bot)))
(define (nonnegative?/pre*) (η/pre* (nonnegative?/pre)))
(define (nonnegative?/idx) any/idx)

;; ===================================================================================================
;; Comparison lifts

(define (</bot*) (η/bot* (</bot)))
(define (</pre*) (η/pre* (</pre)))
(define (</idx) any/idx)

(define (>/bot*) (η/bot* (>/bot)))
(define (>/pre*) (η/pre* (>/pre)))
(define (>/idx) any/idx)

(define (<=/bot*) (η/bot* (<=/bot)))
(define (<=/pre*) (η/pre* (<=/pre)))
(define (<=/idx) any/idx)

(define (>=/bot*) (η/bot* (>=/bot)))
(define (>=/pre*) (η/pre* (>=/pre)))
(define (>=/idx) any/idx)

(define (=/bot*) (η/bot* (=/bot)))
(define (=/pre*) (η/pre* (=/pre)))
(define (=/idx) any/idx)

#lang typed/racket

(require "set-ops.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Set-theoretic mappings (i.e. sets of input-output pairs)

(define-type (Mapping X Y) (Setof (Pair X Y)))

(: mapping (All (X Y) ((X -> Y) (Setof X) -> (Mapping X Y))))
(define (mapping f A)
  (set-image (λ: ([x : X]) (cons x (f x))) A))

(: mapping-domain (All (X Y) ((Mapping X Y) -> (Setof X))))
(define (mapping-domain f)
  (set-image (inst car X Y) f))

(: mapping-range (All (X Y) ((Mapping X Y) -> (Setof Y))))
(define (mapping-range f)
  (set-image (inst cdr X Y) f))

(: mapping-ap (All (X Y) ((Mapping X Y) X -> Y)))
(define (mapping-ap f x)
  (define A (set-filter (λ: ([xy : (Pair X Y)]) (equal? x (car xy))) f))
  (cond [(set-empty? A)  (error 'bad)]
        [else  (cdr (set-take A))]))

(: mapping-image (All (X Y) ((Mapping X Y) (Setof X) -> (Setof Y))))
(define (mapping-image f A)
  (set-image (λ: ([x : X]) (mapping-ap f x))
             (set-intersect A (mapping-domain f))))

(: mapping-preimage (All (X Y) ((Mapping X Y) (Setof Y) -> (Setof X))))
(define (mapping-preimage f B)
  (set-preimage (λ: ([x : X]) (mapping-ap f x))
                (mapping-domain f)
                B))

(: mapping-compose (All (X Y Z) ((Mapping Y Z) (Mapping X Y) -> (Mapping X Z))))
(define (mapping-compose g2 g1)
  (mapping (λ (x) (mapping-ap g2 (mapping-ap g1 x)))
           (mapping-preimage g1 (mapping-domain g2))))

(: mapping-pair (All (X Y Z) ((Mapping X Y) (Mapping X Z) -> (Mapping X (Pair Y Z)))))
(define (mapping-pair g1 g2)
  (mapping (λ (x) (cons (mapping-ap g1 x) (mapping-ap g2 x)))
           (set-intersect (mapping-domain g1) (mapping-domain g2))))

(: mapping-disjoint-union (All (X Y) ((Mapping X Y) (Mapping X Y) -> (Mapping X Y))))
(define (mapping-disjoint-union g1 g2)
  (define A (set-disjoint-union (mapping-domain g1) (mapping-domain g2)))
  (set-union g1 g2))

(: mapping-restrict (All (X Y) ((Mapping X Y) (Setof X) -> (Mapping X Y))))
(define (mapping-restrict f A)
  (set-filter (λ: ([xy : (Pair X Y)]) (set-member? A (car xy))) f))

(: mapping-range-restrict (All (X Y) ((Mapping X Y) (Setof Y) -> (Mapping X Y))))
(define (mapping-range-restrict f B)
  (set-filter (λ: ([xy : (Pair X Y)]) (set-member? B (cdr xy))) f))

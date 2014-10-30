#lang typed/racket

(require "branch-trace.rkt"
         "preimage-mapping.rkt"
         "map-arrow.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Preimage arrow

(define-type (Pre-Arrow X Y) ((Setof X) -> (pmapping X Y)))

(: lift/pre (All (X Y) ((Map-Arrow X Y) -> (Pre-Arrow X Y))))
(define ((lift/pre g) A)
  (pre (g A)))

(: arr/pre (All (X Y) ((X -> Y) -> (Pre-Arrow X Y))))
(define (arr/pre f)
  (lift/pre ((inst arr/map X Y) f)))

(: >>>/pre (All (X Y Z) ((Pre-Arrow X Y) (Pre-Arrow Y Z) -> (Pre-Arrow X Z))))
(define ((h1 . >>>/pre . h2) A)
  (let* ([h1  (h1 A)]
         [h2  (h2 (pmapping-range h1))])
    (pmapping-compose h2 h1)))

(: &&&/pre (All (X Y Z) ((Pre-Arrow X Y) (Pre-Arrow X Z) -> (Pre-Arrow X (Pair Y Z)))))
(define ((h1 . &&&/pre . h2) A)
  (pmapping-pair (h1 A) (h2 A)))

(: ifte/pre (All (X Y) ((Pre-Arrow X Boolean) (Pre-Arrow X Y) (Pre-Arrow X Y) -> (Pre-Arrow X Y))))
(define ((ifte/pre c t f) A)
  (let* ([c  (c A)]
         [t  (t (pmapping-ap c (set #t)))]
         [f  (f (pmapping-ap c (set #f)))])
    (pmapping-disjoint-union t f)))

(: lazy/pre (All (X Y) ((-> (Pre-Arrow X Y)) -> (Pre-Arrow X Y))))
(define ((lazy/pre h) A)
  (if (set-empty? A) (pmapping ((inst set Y)) (λ: ([B : (Setof Y)]) ((inst set X)))) ((h) A)))

;; ---------------------------------------------------------------------------------------------------
;; Preimage arrow lifts

(: id/pre (All (X) (Pre-Arrow X X)))
(define (id/pre A)
  (((inst lift/pre X X) id/map) A))

(: const/pre (All (X Y) (Y -> (Pre-Arrow X Y))))
(define ((const/pre y) A)
  (((inst lift/pre X Y) (const/map y)) A))

(: fst/pre (All (X Y) (Pre-Arrow (Pair X Y) X)))
(define (fst/pre A)
  (((inst lift/pre (Pair X Y) X) fst/map) A))

(: snd/pre (All (X Y) (Pre-Arrow (Pair X Y) Y)))
(define (snd/pre A)
  (((inst lift/pre (Pair X Y) Y) snd/map) A))

(: agrees/pre (Pre-Arrow (Pair Boolean Boolean) Boolean))
(define agrees/pre
  (lift/pre agrees/map))

(: π/pre (Tree-Index -> (Pre-Arrow Branch-Trace Boolean)))
(define (π/pre j)
  (lift/pre (π/map j)))

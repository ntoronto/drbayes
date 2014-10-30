#lang typed/racket

(require "../types.rkt"
         "set-ops.rkt"
         "branch-trace.rkt"
         "mapping.rkt"
         "bot-arrow.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Mapping arrow

(define-type (Map-Arrow X Y) ((Setof X) -> (Mapping X Y)))

(: domain/bot (All (X Y) ((Bot-Arrow X Y) (Setof X) -> (Setof X))))
(define (domain/bot f A)
  (mapping-preimage (mapping f A) (set-subtract (set-image f A) (set ⊥))))

(: lift/map (All (X Y) ((Bot-Arrow X Y) -> (Map-Arrow X Y))))
(define ((lift/map f) A)
  ;(mapping f (domain/bot f A))
  (set-filter-out
   ⊥? (set-image (λ: ([x : X])
                   (define y (f x))
                   (if (⊥? y) y (cons x (just-value y))))
                 A)))

(: arr/map (All (X Y) ((X -> Y) -> (Map-Arrow X Y))))
(define (arr/map f)
  (lift/map (arr/bot f)))

(: >>>/map (All (X Y Z) ((Map-Arrow X Y) (Map-Arrow Y Z) -> (Map-Arrow X Z))))
(define ((g1 . >>>/map . g2) A)
  (let* ([g1  (g1 A)]
         [g2  (g2 (mapping-range g1))])
    (mapping-compose g2 g1)))

(: &&&/map (All (X Y Z) ((Map-Arrow X Y) (Map-Arrow X Z) -> (Map-Arrow X (Pair Y Z)))))
(define ((g1 . &&&/map . g2) A)
  (mapping-pair (g1 A) (g2 A)))

(: ifte/map (All (X Y) ((Map-Arrow X Boolean) (Map-Arrow X Y) (Map-Arrow X Y) -> (Map-Arrow X Y))))
(define ((ifte/map c t f) A)
  (let* ([c  (c A)]
         [t  (t (mapping-preimage c (set #t)))]
         [f  (f (mapping-preimage c (set #f)))])
    (mapping-disjoint-union t f)))

(: lazy/map (All (X Y) ((-> (Map-Arrow X Y)) -> (Map-Arrow X Y))))
(define ((lazy/map g) A)
  (if (set-empty? A) (set) ((g) A)))

;; ---------------------------------------------------------------------------------------------------
;; Mapping arrow lifts

(: id/map (All (X) (Map-Arrow X X)))
(define (id/map A)
  (((inst arr/map X X) (λ: ([x : X]) x)) A))

(: const/map (All (X Y) (Y -> (Map-Arrow X Y))))
(define ((const/map y) A)
  (((inst arr/map X Y) (λ: ([x : X]) y)) A))

(: fst/map (All (X Y) (Map-Arrow (Pair X Y) X)))
(define (fst/map A)
  (((inst arr/map (Pair X Y) X) car) A))

(: snd/map (All (X Y) (Map-Arrow (Pair X Y) Y)))
(define (snd/map A)
  (((inst arr/map (Pair X Y) Y) cdr) A))

(: agrees/map (Map-Arrow (Pair Boolean Boolean) Boolean))
(define agrees/map
  (lift/map agrees/bot))

(: π/map (Tree-Index -> (Map-Arrow Branch-Trace Boolean)))
(define (π/map j)
  (lift/map (π/bot j)))

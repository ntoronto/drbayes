#lang typed/racket

(require "set-ops.rkt"
         "mapping.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Preimage mappings

(struct: (X Y) pmapping ([range : (Setof Y)]
                         [fun : ((Setof Y) -> (Setof X))])
  #:transparent)

(: pre (All (X Y) ((Mapping X Y) -> (pmapping X Y))))
(define (pre g)
  (pmapping (mapping-range g) (λ: ([B : (Setof Y)]) (mapping-preimage g B))))

(: pmapping-ap (All (X Y) ((pmapping X Y) (Setof Y) -> (Setof X))))
(define (pmapping-ap h B)
  (match-define (pmapping Y p) h)
  (p (set-intersect B Y)))

(: pmapping-pair (All (X Y Z) ((pmapping X Y) (pmapping X Z)
                                              -> (pmapping X (Pair Y Z)))))
(define (pmapping-pair h1 h2)
  (match-define (pmapping Y1 p1) h1)
  (match-define (pmapping Y2 p2) h2)
  (define Y (set-product Y1 Y2))
  (define p (λ: ([B : (Setof (Pair Y Z))])
              (set-bind B (λ: ([y : (Pair Y Z)])
                            (match-define (cons y1 y2) y)
                            (set-intersect (p1 (set y1)) (p2 (set y2)))))))
  (pmapping Y p))

(: pmapping-compose (All (X Y Z) ((pmapping Y Z) (pmapping X Y)
                                                 -> (pmapping X Z))))
(define (pmapping-compose h2 h1)
  (match-define (pmapping Z p2) h2)
  (pmapping Z (λ: ([C : (Setof Z)]) (pmapping-ap h1 (p2 C)))))

(: pmapping-disjoint-union (All (X Y) ((pmapping X Y) (pmapping X Y)
                                                      -> (pmapping X Y))))
(define (pmapping-disjoint-union h1 h2)
  (define Y1 (pmapping-range h1))
  (define Y2 (pmapping-range h2))
  (define Y (set-union Y1 Y2))
  (define p (λ: ([B : (Setof Y)])
              (set-disjoint-union (pmapping-ap h1 B) (pmapping-ap h2 B))))
  (pmapping Y p))

(: pmapping-equal? (All (X Y) ((pmapping X Y) (pmapping X Y) -> Boolean)))
(define (pmapping-equal? h1 h2)
  (define B (set-union (pmapping-range h1) (pmapping-range h2)))
  (for/and: ([B  (in-list (set->list (set-power B)))])
    (equal? (pmapping-ap h1 B) (pmapping-ap h2 B))))

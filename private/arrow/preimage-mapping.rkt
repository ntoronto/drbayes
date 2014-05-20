#lang typed/racket/base

(require racket/match
         "../set.rkt"
         "../untyped-utils.rkt")

(provide (all-defined-out))

(define-singleton-type Empty-Pre-Mapping empty-pre-mapping)

(struct: nonempty-pre-mapping ([range : Nonempty-Set]
                               [fun : (Nonempty-Set -> Set)])
  #:transparent)

(define-type Pre-Mapping (U Empty-Pre-Mapping nonempty-pre-mapping))

(: pre-mapping (Set (Nonempty-Set -> Set) -> Pre-Mapping))
(define (pre-mapping Y p)
  (cond [(empty-set? Y)  empty-pre-mapping]
        [else  (nonempty-pre-mapping Y p)]))

(: ap/pre (Pre-Mapping Set -> Set))
(define (ap/pre h B)
  (match h
    [(? empty-pre-mapping?)      empty-set]
    [(nonempty-pre-mapping Y p)  (let ([B  (set-intersect B Y)])
                                   (if (empty-set? B) empty-set (p B)))]))

(: range/pre (Pre-Mapping -> Set))
(define (range/pre h)
  (cond [(empty-pre-mapping? h)  empty-set]
        [else  (nonempty-pre-mapping-range h)]))

(: fun/pre (Pre-Mapping -> (Nonempty-Set -> Set)))
(define (fun/pre h)
  (cond [(empty-pre-mapping? h)  (λ (B) empty-set)]
        [else  (nonempty-pre-mapping-fun h)]))

(: compose/pre (Pre-Mapping Pre-Mapping -> Pre-Mapping))
(define (compose/pre h2 h1)
  (cond [(or (empty-pre-mapping? h1) (empty-pre-mapping? h2))  empty-pre-mapping]
        [else  (match-define (nonempty-pre-mapping Z p2) h2)
               (nonempty-pre-mapping Z (λ (C) (ap/pre h1 (p2 C))))]))

(: pair/pre (Pre-Mapping Pre-Mapping -> Pre-Mapping))
(define (pair/pre h1 h2)
  (cond [(or (empty-pre-mapping? h1) (empty-pre-mapping? h2))  empty-pre-mapping]
        [else  (match-define (nonempty-pre-mapping Y1 p1) h1)
               (match-define (nonempty-pre-mapping Y2 p2) h2)
               (nonempty-pre-mapping
                (set-pair Y1 Y2)
                (λ (B)
                  (define-values (B1 B2) (set-projs B))
                  (cond [(or (empty-set? B1) (empty-set? B2))  empty-set]
                        [else  (define A1 (p1 B1))
                               (cond [(empty-set? A1)  empty-set]
                                     [else  (set-intersect A1 (p2 B2))])])))]))

(: uplus/pre (Pre-Mapping Pre-Mapping -> Pre-Mapping))
(define (uplus/pre h1 h2)
  (cond [(and (empty-pre-mapping? h1) (empty-pre-mapping? h2))  empty-pre-mapping]
        [(empty-pre-mapping? h1)  h2]
        [(empty-pre-mapping? h2)  h1]
        #;; Direct implementation from the paper:
        [else  (nonempty-pre-mapping
                (set-join (nonempty-pre-mapping-range h1)
                          (nonempty-pre-mapping-range h2))
                (λ (B) (set-join (ap/pre h1 B)
                                 (ap/pre h2 B))))]
        ;; Less direct implementation that computes tighter preimages (should this be in the paper?)
        [else
         (define Y1 (nonempty-pre-mapping-range h1))
         (define Y2 (nonempty-pre-mapping-range h2))
         (nonempty-pre-mapping
          (set-join Y1 Y2)
          (λ (B) (set-join (ap/pre h1 (set-intersect Y1 B))
                           (ap/pre h2 (set-intersect Y2 B)))))]))

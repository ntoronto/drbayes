#lang typed/racket/base

(require racket/match
         "../set.rkt"
         "types.rkt"
         "cache.rkt")

(provide (all-defined-out))

(: pre-mapping (Set (Nonempty-Set -> Set) -> Pre-Mapping))
(define (pre-mapping Y p)
  (if (empty-set? Y) empty-pre-mapping (nonempty-pre-mapping Y p)))

(: preimage/pre (Pre-Mapping Set -> Set))
(define (preimage/pre h B)
  (if (empty-pre-mapping? h)
      empty-set
      (let ([B  (set-intersect B (nonempty-pre-mapping-range h))])
        (if (empty-set? B)
            empty-set
            ((nonempty-pre-mapping-fun h) B)))))

(: range/pre (Pre-Mapping -> Set))
(define (range/pre h)
  (if (empty-pre-mapping? h) empty-set (nonempty-pre-mapping-range h)))

(: make-compose/pre (-> (-> Pre-Mapping Pre-Mapping Pre-Mapping)))
(define (make-compose/pre)
  (define fun (make-pre-mapping-fun/memo))
  (λ (h2 h1)
    (cond [(or (empty-pre-mapping? h1) (empty-pre-mapping? h2))  empty-pre-mapping]
          [else  (match-define (nonempty-pre-mapping Z p2) h2)
                 (nonempty-pre-mapping Z (fun (λ (C) (preimage/pre h1 (p2 C)))))])))

(: make-pair/pre (-> (-> Pre-Mapping Pre-Mapping Pre-Mapping)))
(define (make-pair/pre)
  (define pair (make-nonempty-set-pair/memo))
  (define fun (make-pre-mapping-fun/memo))
  (λ (h1 h2)
    (cond [(or (empty-pre-mapping? h1) (empty-pre-mapping? h2))  empty-pre-mapping]
          [else
           (match-define (nonempty-pre-mapping Y1 p1) h1)
           (match-define (nonempty-pre-mapping Y2 p2) h2)
           (nonempty-pre-mapping
            (pair Y1 Y2)
            (fun (λ (B)
                   (define-values (B1 B2) (set-projs B))
                   (if (or (empty-set? B1) (empty-set? B2))
                       empty-set
                       (let ([A1  (p1 B1)])
                         (if (empty-set? A1)
                             empty-set
                             (set-intersect A1 (p2 B2))))))))])))

(: make-uplus/pre (-> (-> Pre-Mapping Pre-Mapping Pre-Mapping)))
(define (make-uplus/pre)
  (define fun (make-pre-mapping-fun/memo))
  (λ (h1 h2)
    (cond
      [(empty-pre-mapping? h1)  h2]
      [(empty-pre-mapping? h2)  h1]
      [else
       (define Y1 (nonempty-pre-mapping-range h1))
       (define Y2 (nonempty-pre-mapping-range h2))
       (define Y (set-join Y1 Y2))
       #;; More or less direct implementation:
       (nonempty-pre-mapping Y (fun (λ (B) (set-join (preimage/pre h1 B)
                                                     (preimage/pre h2 B)))))
       ;; Implementation that computes tighter preimages:
       (nonempty-pre-mapping Y (fun (λ (B) (set-join (preimage/pre h1 (set-intersect Y1 B))
                                                     (preimage/pre h2 (set-intersect Y2 B))))))])))

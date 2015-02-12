#lang typed/racket/base

(require racket/match
         "../set.rkt"
         "types.rkt"
         "cache.rkt")

(provide (all-defined-out))

(: pre-mapping (-> Set (-> Nonempty-Set (Values Set Boolean)) Pre-Mapping))
(define (pre-mapping Y p)
  (if (empty-set? Y) empty-pre-mapping (nonempty-pre-mapping Y p)))

(: preimage/pre (-> Pre-Mapping Set (Values Set Boolean)))
(define (preimage/pre h B)
  (if (empty-pre-mapping? h)
      (values empty-set #t)
      (let ([B  (set-intersect B (nonempty-pre-mapping-range h))])
        (if (empty-set? B)
            (values empty-set #t)
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
                 (nonempty-pre-mapping
                  Z (fun (λ (C)
                           (define-values (B B-exact?) (p2 C))
                           (define-values (A A-exact?) (preimage/pre h1 B))
                           (values A (and B-exact? A-exact?)))))])))

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
                       (values empty-set #t)
                       (let-values ([(A1 A1-exact?)  (p1 B1)])
                         (if (empty-set? A1)
                             (values empty-set #t)
                             (let-values ([(A2 A2-exact?)  (p2 B2)])
                               (if (empty-set? A2)
                                   (values empty-set #t)
                                   (let ([A  (set-intersect A1 A2)])
                                     (if (empty-set? A)
                                         (values empty-set #t)
                                         (values A (and A1-exact? A2-exact?))))))))))))])))

(: make-uplus/pre (-> (-> Boolean Boolean Pre-Mapping Pre-Mapping Pre-Mapping)))
(define (make-uplus/pre)
  (define fun (make-pre-mapping-fun/memo))
  (λ (At-exact? Af-exact? ht hf)
    (cond
      [(and (empty-pre-mapping? ht) (empty-pre-mapping? hf))  empty-pre-mapping]
      [(empty-pre-mapping? hf)
       (define Y (nonempty-pre-mapping-range ht))
       (nonempty-pre-mapping
        Y (fun (λ (B)
                 (define-values (A A-exact?) (preimage/pre ht B))
                 (values A (and At-exact? A-exact?)))))]
      [(empty-pre-mapping? ht)
       (define Y (nonempty-pre-mapping-range hf))
       (nonempty-pre-mapping
        Y (fun (λ (B)
                 (define-values (A A-exact?) (preimage/pre hf B))
                 (values A (and Af-exact? A-exact?)))))]
      [else
       (define Yt (nonempty-pre-mapping-range ht))
       (define Yf (nonempty-pre-mapping-range hf))
       (define-values (Y Y-exact?) (set-join Yt Yf))
       (nonempty-pre-mapping
        Y (fun (λ (B)
                 (define-values (A2 A2-exact?) (preimage/pre ht (set-intersect Yt B)))
                 (define-values (A3 A3-exact?) (preimage/pre hf (set-intersect Yf B)))
                 (define-values (A A-exact?) (set-join A2 A3))
                 (values A (and At-exact? Af-exact? A-exact? A2-exact? A3-exact?)))))])))

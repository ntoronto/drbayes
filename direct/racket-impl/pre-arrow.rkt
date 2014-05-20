#lang typed/racket/base

(require racket/promise
         "set.rkt"
         "pre-mapping.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Preimage arrow

(define-type Pre-Arrow (Set -> pre-mapping))

(: >>>/pre (Pre-Arrow Pre-Arrow -> Pre-Arrow))
(define ((>>>/pre h1 h2) A)
  (let* ([h1  (h1 A)]
         [h2  (h2 (pre-range h1))])
    (pre-comp h2 h1)))

(: &&&/pre (Pre-Arrow Pre-Arrow -> Pre-Arrow))
(define ((&&&/pre h1 h2) A)
  (pre-pair (h1 A) (h2 A)))

(: if/pre (Pre-Arrow Pre-Arrow Pre-Arrow -> Pre-Arrow))
(define ((if/pre h1 h2 h3) A)
  (let ([h1  (h1 A)])
    (pre-plus (h2 (pre-ap h1 true-set))
              (h3 (pre-ap h1 false-set)))))

(: lazy/pre ((Promise Pre-Arrow) -> Pre-Arrow))
(define ((lazy/pre h) A)
  (if (empty-set? A) empty-pre-mapping ((force h) A)))

(: id/pre Pre-Arrow)
(define (id/pre A)
  (pre-mapping A (λ: ([B : Set]) B)))

(: const/pre (Value -> Pre-Arrow))
(define ((const/pre b) A)
  (pre-mapping (set-singleton b) (λ: ([B : Set]) (if (empty-set? B) empty-set A))))

(: fst/pre Pre-Arrow)
(define (fst/pre A)
  (define A2 (set-proj-snd A))
  (pre-mapping (set-proj-fst A) (λ: ([B : Set]) (set-meet A (set-prod B A2)))))

(: snd/pre Pre-Arrow)
(define (snd/pre A)
  (define A1 (set-proj-fst A))
  (pre-mapping (set-proj-snd A) (λ: ([B : Set]) (set-meet A (set-prod A1 B)))))

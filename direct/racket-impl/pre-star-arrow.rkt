#lang typed/racket/base

(require racket/promise
         racket/match
         "set.rkt"
         "pre-mapping.rkt"
         "pre-arrow.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Preimage arrow

(define-type Pre*-Arrow (Tree-Index -> Pre-Arrow))

(: lift/pre* (Pre-Arrow -> Pre*-Arrow))
(define ((lift/pre* h) j)
  (snd/pre . >>>/pre . h))

(: >>>/pre* (Pre*-Arrow Pre*-Arrow -> Pre*-Arrow))
(define ((>>>/pre* k1 k2) j)
  ((fst/pre . &&&/pre . (k1 (index-left j))) . >>>/pre . (k2 (index-right j))))

(: &&&/pre* (Pre*-Arrow Pre*-Arrow -> Pre*-Arrow))
(define ((&&&/pre* k1 k2) j)
  ((k1 (index-left j)) . &&&/pre . (k2 (index-right j))))

(: if/pre* (Pre*-Arrow Pre*-Arrow Pre*-Arrow -> Pre*-Arrow))
(define ((if/pre* k1 k2 k3) j)
  (if/pre (k1 (index-left j))
          (k2 (index-left (index-right j)))
          (k3 (index-right (index-right j)))))

(: lazy/pre* ((Promise Pre*-Arrow) -> Pre*-Arrow))
(define ((lazy/pre* k) j)
  (lazy/pre (delay ((force k) j))))

(: id/pre* Pre*-Arrow)
(define id/pre* (lift/pre* id/pre))

(: const/pre* (Value -> Pre*-Arrow))
(define (const/pre* b) (lift/pre* (const/pre b)))

(: fst/pre* Pre*-Arrow)
(define fst/pre* (lift/pre* fst/pre))

(: snd/pre* Pre*-Arrow)
(define snd/pre* (lift/pre* snd/pre))

;; ---------------------------------------------------------------------------------------------------
;; Random numbers

(: random/pre (Tree-Index -> Pre-Arrow))
(define ((random/pre j) A)
  (pre-mapping (set-meet unit-ivl (set-project j A))
               (λ: ([B : Set]) (set-unproject j A B))))

(: random/pre* Pre*-Arrow)
(define (random/pre* j)
  ((fst/pre . >>>/pre . fst/pre) . >>>/pre . (random/pre j)))

;; ---------------------------------------------------------------------------------------------------
;; Conditional that always converges

(: branch/pre (Tree-Index -> Pre-Arrow))
(define ((branch/pre j) A)
  (pre-mapping (set-meet bools-set (set-project j A))
               (λ: ([B : Set]) (set-unproject j A B))))

(: branch/pre* Pre*-Arrow)
(define (branch/pre* j)
  (fst/pre . >>>/pre . (snd/pre . >>>/pre . (branch/pre j))))

(: convif/pre* (Pre*-Arrow Pre*-Arrow Pre*-Arrow -> Pre*-Arrow))
(define (((convif/pre* k1 k2 k3) j) A)
  (match-define (pre-mapping Ck pk) ((k1 (index-left j)) A))
  (match-define (pre-mapping Cb pb) ((branch/pre* j) A))
  (define C (set-meet Ck Cb))
  (define C2 (set-meet C true-set))
  (define C3 (set-meet C false-set))
  (define A2 (set-meet (pk C2) (pb C2)))
  (define A3 (set-meet (pk C3) (pb C3)))
  (cond [(bools-set? Cb)
         (define A (set-join A2 A3))
         (pre-mapping univ-set (λ: ([B : Set]) (if (empty-set? B) empty-set A)))]
        [else
         (pre-plus ((k2 (index-left (index-right j))) A2)
                   ((k3 (index-right (index-right j))) A3))]))

;; ===================================================================================================

(pre-ap ((random/pre* j0) (set-prod (set-prod univ-tree-set univ-tree-set) null-set))
        (ivl 0.0 0.5))

(: halt-on-true/pre* Pre*-Arrow)
(define halt-on-true/pre*
  (convif/pre* id/pre* id/pre* (lazy/pre* (delay halt-on-true/pre*))))

(pre-ap ((halt-on-true/pre* j0) univ-set) true-set)
(pre-ap ((halt-on-true/pre* j0) univ-set) false-set)
(pre-ap ((halt-on-true/pre* j0) univ-set) bools-set)

(pre-ap
 ((halt-on-true/pre* j0) (set-prod (set-prod univ-tree-set
                                             (set-unproject '() univ-tree-set false-set))
                                   bools-set))
 bools-set)


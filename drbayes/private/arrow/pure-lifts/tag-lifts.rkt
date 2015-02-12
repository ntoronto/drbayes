#lang typed/racket/base

(require racket/promise
         "../../set.rkt"
         "../types.rkt"
         "../cache.rkt"
         "make-predicate-lift.rkt"
         )

(provide (all-defined-out))

;; ===================================================================================================
;; Tag predicate lifts

(: tag?/bot (-> Tag Bot-Arrow))
(define ((tag?/bot tag) a) (and (tagged-value? a) (eq? tag (tagged-value-tag a))))

(: tag?/pre (-> Tag Pre-Arrow))
(define (tag?/pre tag) ((predicate/pre (bot-tagged tag universe) (top-tagged tag empty-set))))

;; ===================================================================================================
;; Tagging lifts

(: tag/bot (-> Tag Bot-Arrow))
(define ((tag/bot tag) a) (tagged-value tag a))

(: tag/pre (-> Tag Pre-Arrow))
(define (tag/pre tag)
  (define fun (make-pre-mapping-fun/memo))
  (make-pre-arrow/memo
   (λ (A) (nonempty-pre-mapping (set-tag A tag) (fun (λ (B) (values (set-untag B tag) #t)))))))

;; ===================================================================================================
;; Untagging lifts

(: untag/bot (-> Tag Bot-Arrow))
(define ((untag/bot tag) a)
  (if (and (tagged-value? a) (eq? tag (tagged-value-tag a)))
      (tagged-value-value a)
      (bottom (delay (format "expected ~a; given ~e" tag a)))))

(: untag/pre (-> Tag Pre-Arrow))
(define (untag/pre tag)
  (define fun (make-pre-mapping-fun/memo))
  (make-pre-arrow/memo
   (λ (A)
     (define B (set-untag A tag))
     (cond [(empty-set? B)  empty-pre-mapping]
           [else  (nonempty-pre-mapping B (fun (λ (B) (values (set-tag B tag) #t))))]))))

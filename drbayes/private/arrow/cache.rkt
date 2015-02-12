#lang typed/racket/base

(require "../set.rkt"
         "types.rkt"
         "parameters.rkt")

(provide (all-defined-out))

;; Parameter reads are slow, so cache them
(define-syntax-rule (cached-boolean-parameter param)
  (let ([value : (U Boolean 'any)  'any])
    (λ () (cond [(eq? value #f)  #f]
                [(eq? value #t)  #t]
                [else  (set! value (param))
                       value]))))

(: make-pre-arrow/memo (-> (-> Nonempty-Set Pre-Mapping) Pre-Arrow))
(define (make-pre-arrow/memo h)
  (define cache? (cached-boolean-parameter drbayes-image-cache?))
  (define check? (cached-boolean-parameter drbayes-image-cache-check-bad-misses?))
  
  (: last-A Set)
  (: last-pre Pre-Mapping)
  (define last-A empty-set)
  (define last-pre empty-pre-mapping)
  
  (λ (A)
    (cond [(not (cache?))
           (h A)]
          [(eq? A last-A)
           (register-image-cache-hit!)
           last-pre]
          [else
           (register-image-cache-miss!)
           (when (and (check?) (equal? A last-A))
             (register-image-cache-bad-miss! (set-cache-key A)))
           (define pre (h A))
           (set! last-A A)
           (set! last-pre pre)
           pre])))

(: make-nonempty-set-pair/memo (-> (-> Nonempty-Set Nonempty-Set Nonempty-Set)))
(define (make-nonempty-set-pair/memo)
  (define cache? (cached-boolean-parameter drbayes-pair-cache?))
  (define check? (cached-boolean-parameter drbayes-pair-cache-check-bad-misses?))
  
  (: last-A1 (U #f Nonempty-Set))
  (: last-A2 (U #f Nonempty-Set))
  (: last-A Nonempty-Set)
  (define last-A1 #f)
  (define last-A2 #f)
  (define last-A universe)
  
  (λ (A1 A2)
    (cond [(not (cache?))
           (if (and (universe? A1) (universe? A2))
               pairs
               (plain-pair-set A1 A2))]
          [(and (eq? A1 last-A1) (eq? A2 last-A2))
           (register-pair-cache-hit!)
           last-A]
          [else
           (register-pair-cache-miss!)
           (when (and (check?) (let ([equal1?  (equal? A1 last-A1)]
                                     [equal2?  (equal? A2 last-A2)])
                                 (or (and (eq? A1 last-A1) equal2?)
                                     (and equal1? (eq? A2 last-A2))
                                     (and equal1? equal2?))))
             (register-pair-cache-bad-miss! (list (set-cache-key A1)
                                                  (set-cache-key A2))))
           (define A (set-pair A1 A2))
           (set! last-A1 A1)
           (set! last-A2 A2)
           (set! last-A A)
           A])))

(: make-pre-mapping-fun/memo (-> (-> (-> Nonempty-Set (Values Set Boolean))
                                     (-> Nonempty-Set (Values Set Boolean)))))
(define (make-pre-mapping-fun/memo)
  (define cache? (cached-boolean-parameter drbayes-preimage-cache?))
  (define check? (cached-boolean-parameter drbayes-preimage-cache-check-bad-misses?))
  (λ (p)
    (: last-B (U #f Nonempty-Set))
    (: last-A Set)
    (: last-exact? Boolean)
    (define last-B #f)
    (define last-A empty-set)
    (define last-exact? #t)
    (λ (B)
      (cond [(not (cache?))
             (p B)]
            [(eq? B last-B)
             (register-preimage-cache-hit!)
             (values last-A last-exact?)]
            [else
             (register-preimage-cache-miss!)
             (when (and (check?) (equal? B last-B))
               (register-preimage-cache-bad-miss! (set-cache-key B)))
             (define-values (A exact?) (p B))
             (set! last-B B)
             (set! last-A A)
             (set! last-exact? exact?)
             (values A exact?)]))))

#lang typed/racket/base

(require racket/list
         racket/flonum
         "../../set.rkt"
         "../types.rkt"
         "../cache.rkt"
         "../pure-arrows.rkt"
         "make-real-lift.rkt"
         )

(provide </bot >/bot <=/bot >=/bot =/bot
         </pre >/pre <=/pre >=/pre =/pre)

(define </bot (real2d/bot '< reals reals bools fl<))
(define >/bot (real2d/bot '> reals reals bools fl>))
(define <=/bot (real2d/bot '<= reals reals bools fl<=))
(define >=/bot (real2d/bot '>= reals reals bools fl>=))
(define =/bot (real2d/bot '= reals reals bools fl=))

(: real-set-first (-> Nonempty-Real-Set Nonempty-Real-Interval))
(define (real-set-first A)
  (cond [(reals? A)  A]
        [(Plain-Real-Interval? A)  A]
        [else  (first (Plain-Real-Interval-List-elements A))]))

(: real-set-last (-> Nonempty-Real-Set Nonempty-Real-Interval))
(define (real-set-last A)
  (cond [(reals? A)  A]
        [(Plain-Real-Interval? A)  A]
        [else  (last (Plain-Real-Interval-List-elements A))]))

(: real-set-lt? (-> Nonempty-Real-Set Nonempty-Real-Set Boolean))
(define (real-set-lt? A B)
  (define-values (a1 a2 a1? a2?) (real-interval-fields (real-set-last A)))
  (define-values (b1 b2 b1? b2?) (real-interval-fields (real-set-first B)))
  (or (fl< a2 b1) (and (fl= a2 b1) (not (and a2? b1?)))))

(: real-set-gte? (-> Nonempty-Real-Set Nonempty-Real-Set Boolean))
(define (real-set-gte? A B)
  (define-values (a1 a2 a1? a2?) (real-interval-fields (real-set-first A)))
  (define-values (b1 b2 b1? b2?) (real-interval-fields (real-set-last B)))
  (fl<= b2 a1))

(: bool-set-not (-> Bool-Set Bool-Set))
(define (bool-set-not A)
  (cond [(trues? A)  falses]
        [(falses? A)  trues]
        [else  A]))

(: lt-img (-> Set Bool-Set))
(define (lt-img AB)
  (define-values (A B) (set-projs AB))
  (let ([A  (set-take-reals A)]
        [B  (set-take-reals B)])
    (cond [(or (empty-real-set? A) (empty-real-set? B))  empty-bool-set]
          [(real-set-lt? A B)  trues]
          [(real-set-gte? A B)  falses]
          [else  bools])))

(: lt-pre-true (-> Set Set))
(define (lt-pre-true AB)
  (define-values (A B) (set-projs AB))
  (real-set-map*
   (λ (A)
     (define-values (a1 a2 a1? a2?) (real-interval-fields A))
     (real-set-map*
      (λ (B)
        (define-values (b1 b2 b1? b2?) (real-interval-fields B))
        (set-pair (set-intersect A (bot-basic (real-interval -inf.0 b2 #f #f)))
                  (set-intersect B (bot-basic (real-interval a1 +inf.0 #f #f)))))
      (set-take-reals B)))
   (set-take-reals A)))

(: lt-pre-false (-> Set Set))
(define (lt-pre-false A×B)
  (define-values (A B) (set-projs A×B))
  (real-set-map*
   (λ (A)
     (define-values (a1 a2 a1? a2?) (real-interval-fields A))
     (real-set-map*
      (λ (B)
        (define-values (b1 b2 b1? b2?) (real-interval-fields B))
        (set-pair (set-intersect A (bot-basic (real-interval b1 +inf.0 b1? #f)))
                  (set-intersect B (bot-basic (real-interval -inf.0 a2 #f a2?)))))
      (set-take-reals B)))
   (set-take-reals A)))

(: </pre (-> Pre-Arrow))
(define (</pre)
  (define fun (make-pre-mapping-fun/memo))
  (make-pre-arrow/memo
   (λ (A)
     (define B (lt-img A))
     (cond [(empty-bool-set? B)  empty-pre-mapping]
           [else  (nonempty-pre-mapping
                   B (fun (λ (B)
                            (let ([B  (set-take-bools B)])
                              (cond [(empty-bool-set? B)  empty-set]
                                    [(trues? B)  (lt-pre-true A)]
                                    [(falses? B)  (lt-pre-false A)]
                                    [else  A])))))]))))

(: >=/pre (-> Pre-Arrow))
(define (>=/pre)
  (define fun (make-pre-mapping-fun/memo))
  (make-pre-arrow/memo
   (λ (A)
     (define B (bool-set-not (lt-img A)))
     (cond [(empty-bool-set? B)  empty-pre-mapping]
           [else  (nonempty-pre-mapping
                   B (fun (λ (C)
                            (let ([C  (set-take-bools C)])
                              (cond [(empty-bool-set? C)  empty-set]
                                    [(trues? C)  (lt-pre-false A)]
                                    [(falses? C)  (lt-pre-true A)]
                                    [else  A])))))]))))

(: swap/pre (-> Pre-Arrow))
(define (swap/pre)
  (define fun (make-pre-mapping-fun/memo))
  (make-pre-arrow/memo
   (λ (A)
     (define-values (A1 A2) (set-projs A))
     (define B (pair-set A2 A1))
     (cond [(empty-pair-set? B)  empty-pre-mapping]
           [else  (nonempty-pre-mapping
                   B (fun (λ (B)
                            (define-values (B1 B2) (set-projs B))
                            (set-pair B2 B1))))]))))

(define (>/pre) (>>>/pre (swap/pre) (</pre)))
(define (<=/pre) (>>>/pre (swap/pre) (>=/pre)))
(define (=/pre) (ifte/pre (>=/pre) (ifte/pre (<=/pre) (const/pre #t) (const/pre #f)) (const/pre #f)))

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
        [else  (first (Real-Interval-List-elements A))]))

(: real-set-last (-> Nonempty-Real-Set Nonempty-Real-Interval))
(define (real-set-last A)
  (cond [(reals? A)  A]
        [(Plain-Real-Interval? A)  A]
        [else  (last (Real-Interval-List-elements A))]))

(: real-set-lt? (-> Nonempty-Real-Set Nonempty-Real-Set Boolean))
(define (real-set-lt? A B)
  (define-values (a1 a2 a1? a2?) (real-interval-fields (real-set-last A)))
  (define-values (b1 b2 b1? b2?) (real-interval-fields (real-set-first B)))
  (or (< a2 b1) (and (= a2 b1) (not (and a2? b1?)))))

(: real-set-gte? (-> Nonempty-Real-Set Nonempty-Real-Set Boolean))
(define (real-set-gte? A B)
  (define-values (a1 a2 a1? a2?) (real-interval-fields (real-set-first A)))
  (define-values (b1 b2 b1? b2?) (real-interval-fields (real-set-last B)))
  (<= b2 a1))

(: real-set-gt? (-> Nonempty-Real-Set Nonempty-Real-Set Boolean))
(define (real-set-gt? A B)
  (define-values (a1 a2 a1? a2?) (real-interval-fields (real-set-first A)))
  (define-values (b1 b2 b1? b2?) (real-interval-fields (real-set-last B)))
  (or (> a2 b1) (and (= a2 b1) (not (and a2? b1?)))))

(: real-set-lte? (-> Nonempty-Real-Set Nonempty-Real-Set Boolean))
(define (real-set-lte? A B)
  (define-values (a1 a2 a1? a2?) (real-interval-fields (real-set-last A)))
  (define-values (b1 b2 b1? b2?) (real-interval-fields (real-set-first B)))
  (>= b2 a1))

(: bool-set-not (-> Bool-Set Bool-Set))
(define (bool-set-not A)
  (cond [(trues? A)  falses]
        [(falses? A)  trues]
        [else  A]))

(: lt-img (-> Set (Values Bool-Set #t)))
(define (lt-img AB)
  (define-values (A B) (set-projs AB))
  (let ([A  (set-take-reals A)]
        [B  (set-take-reals B)])
    (cond [(or (empty-real-set? A) (empty-real-set? B))  (values empty-bool-set #t)]
          [(real-set-lt? A B)  (values trues #t)]
          [(real-set-gte? A B)  (values falses #t)]
          [else  (values bools #t)])))

(: lt-pre-true (-> Nonempty-Real-Set Nonempty-Real-Set (Values Set Boolean)))
(define (lt-pre-true A1 A2)
  (real-set-map*
   (λ (A1)
     (define-values (a1 b1 a1? b1?) (real-interval-fields A1))
     (real-set-map*
      (λ (A2)
        (define-values (a2 b2 a2? b2?) (real-interval-fields A2))
        (cond
          [(or (< b1 a2) (and (= b1 a2) (not (and b1? a2?))))
           (values (set-pair A1 A2) #t)]
          [(< a1 b2)
           (values (set-pair (if (< b1 b2) A1 (plain-real-interval a1 b2 a1? #f))
                             (if (< a1 a2) A2 (plain-real-interval a1 b2 #f b2?)))
                   #f)]
          [else
           (values empty-set #t)]))
      A2))
   A1))

(: lt-pre-false (-> Nonempty-Real-Set Nonempty-Real-Set (Values Set Boolean)))
(define (lt-pre-false A1 A2)
  (real-set-map*
   (λ (A1)
     (define-values (a1 b1 a1? b1?) (real-interval-fields A1))
     (real-set-map*
      (λ (A2)
        (define-values (a2 b2 a2? b2?) (real-interval-fields A2))
        (cond
          [(or (< b1 a2) (and (= b1 a2) (not (and b1? a2?))))
           (values empty-set #t)]
          [(< a1 b2)
           (values (set-pair 
                    (if (or (< a2 a1) (and (= a2 a1) a2?)) A1 (plain-real-interval a2 b1 a2? b1?))
                    (if (or (< b2 b1) (and (= b2 b1) b1?)) A2 (plain-real-interval a2 b1 a2? b1?)))
                   #f)]
          [else
           (values (set-pair A1 A2) #t)]))
      A2))
   A1))

(: </pre (-> Pre-Arrow))
(define (</pre)
  (define fun (make-pre-mapping-fun/memo))
  (make-pre-arrow/memo
   (λ (A)
     (define-values (A1 A2) (set-projs A))
     (let ([A1  (set-take-reals A1)]
           [A2  (set-take-reals A2)])
       (cond [(or (empty-real-set? A1) (empty-real-set? A2))
              empty-pre-mapping]
             [else
              (define A (set-pair A1 A2))
              (cond [(real-set-lt? A1 A2)   (nonempty-pre-mapping trues  (λ (B) (values A #t)))]
                    [(real-set-gte? A1 A2)  (nonempty-pre-mapping falses (λ (B) (values A #t)))]
                    [else
                     (nonempty-pre-mapping
                      bools
                      (fun (λ (B)
                             (let ([B  (set-take-bools B)])
                               (cond [(empty-bool-set? B)  (values empty-set #t)]
                                     [(trues? B)   (lt-pre-true  A1 A2)]
                                     [(falses? B)  (lt-pre-false A1 A2)]
                                     [else  (values A #t)])))))])])))))

(: >=/pre (-> Pre-Arrow))
(define (>=/pre)
  (define fun (make-pre-mapping-fun/memo))
  (make-pre-arrow/memo
   (λ (A)
     (define-values (A1 A2) (set-projs A))
     (let ([A1  (set-take-reals A1)]
           [A2  (set-take-reals A2)])
       (cond [(or (empty-real-set? A1) (empty-real-set? A2))
              empty-pre-mapping]
             [else
              (define A (set-pair A1 A2))
              (cond [(real-set-lt? A1 A2)   (nonempty-pre-mapping falses (λ (B) (values A #t)))]
                    [(real-set-gte? A1 A2)  (nonempty-pre-mapping trues  (λ (B) (values A #t)))]
                    [else
                     (nonempty-pre-mapping
                      bools
                      (fun (λ (B)
                             (let ([B  (set-take-bools B)])
                               (cond [(empty-bool-set? B)  (values empty-set #t)]
                                     [(trues? B)   (lt-pre-false A1 A2)]
                                     [(falses? B)  (lt-pre-true  A1 A2)]
                                     [else  (values A #t)])))))])])))))

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
                            (values (set-pair B2 B1) #t))))]))))

(define (>/pre) (>>>/pre (swap/pre) (</pre)))
(define (<=/pre) (>>>/pre (swap/pre) (>=/pre)))
(define (=/pre) (ifte/pre (>=/pre) (ifte/pre (<=/pre) (const/pre #t) (const/pre #f)) (const/pre #f)))

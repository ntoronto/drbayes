#lang typed/racket/base

(require racket/match
         racket/list
         racket/promise
         "types.rkt"
         "bottom.rkt"
         "real-set.rkt"
         "bool-set.rkt"
         "store-index.rkt"
         "store.rkt"
         "../untyped-utils.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Unbounded trees used for program store sets

(struct: Base-Store-Set Base-Bot-Basic () #:transparent)
(define store-set? Base-Store-Set?)

(define-singleton-type Empty-Store-Set Base-Store-Set empty-store-set)
(define-singleton-type Full-Store-Set Base-Store-Set stores)

(struct Nonextremal-Store-Set Base-Store-Set
  ([random : Nonextremal-Real-Set]
   [branch : Nonempty-Bool-Set]
   [left   : Nonempty-Store-Set]
   [right  : Nonempty-Store-Set])
  #:transparent)

(define-type Nonempty-Store-Set (U Nonextremal-Store-Set Full-Store-Set))
(define-type Nonfull-Store-Set (U Nonextremal-Store-Set Empty-Store-Set))
(define-type Store-Set (U Nonextremal-Store-Set Empty-Store-Set Full-Store-Set))

(: store-set (-> Real-Set Bool-Set Store-Set Store-Set Store-Set))
(define (store-set X B L R)
  (let ([X  (real-set-intersect unit-interval X)])
    (if (or (empty-real-set? X) (empty-bool-set? B) (empty-store-set? L) (empty-store-set? R))
        empty-store-set
        (Nonextremal-Store-Set X B L R))))

;; ===================================================================================================
;; Simple projections

(: store-set-random (case-> (-> Empty-Store-Set Empty-Real-Set)
                            (-> Nonempty-Store-Set Nonextremal-Real-Set)
                            (-> Store-Set Real-Set)))
(define (store-set-random S)
  (cond [(empty-store-set? S)  empty-real-set]
        [(stores? S)  unit-interval]
        [else  (Nonextremal-Store-Set-random S)]))

(: store-set-branch (case-> (-> Empty-Store-Set Empty-Bool-Set)
                            (-> Nonempty-Store-Set Nonempty-Bool-Set)
                            (-> Store-Set Bool-Set)))
(define (store-set-branch S)
  (cond [(empty-store-set? S)  empty-bool-set]
        [(stores? S)  bools]
        [else  (Nonextremal-Store-Set-branch S)]))

(: store-set-left (case-> (-> Empty-Store-Set Empty-Store-Set)
                          (-> Nonempty-Store-Set Nonempty-Store-Set)
                          (-> Store-Set Store-Set)))
(define (store-set-left S)
  (cond [(empty-store-set? S)  empty-store-set]
        [(stores? S)  stores]
        [else  (Nonextremal-Store-Set-left S)]))

(: store-set-right (case-> (-> Empty-Store-Set Empty-Store-Set)
                           (-> Nonempty-Store-Set Nonempty-Store-Set)
                           (-> Store-Set Store-Set)))
(define (store-set-right S)
  (cond [(empty-store-set? S)  empty-store-set]
        [(stores? S)  stores]
        [else  (Nonextremal-Store-Set-right S)]))

(: store-set-projs (case-> (-> Empty-Store-Set (Values Empty-Real-Set
                                                       Empty-Bool-Set
                                                       Empty-Store-Set
                                                       Empty-Store-Set))
                           (-> Nonempty-Store-Set (Values Nonextremal-Real-Set
                                                          Nonempty-Bool-Set
                                                          Nonempty-Store-Set
                                                          Nonempty-Store-Set))
                           (-> Store-Set (Values Nonfull-Real-Set
                                                 Bool-Set
                                                 Store-Set
                                                 Store-Set))))
(define (store-set-projs S)
  (cond [(empty-store-set? S)  (values empty-real-set empty-bool-set empty-store-set empty-store-set)]
        [(stores? S)  (values unit-interval bools stores stores)]
        [else  (values (Nonextremal-Store-Set-random S)
                       (Nonextremal-Store-Set-branch S)
                       (Nonextremal-Store-Set-left   S)
                       (Nonextremal-Store-Set-right  S))]))

;; ===================================================================================================
;; Simple unprojections

(: store-set-unrandom (-> Store-Set Real-Set Store-Set))
(define (store-set-unrandom S X)
  (define-values (X* B L R) (store-set-projs S))
  (let ([X  (real-set-intersect X* X)])
    (if (eq? X* X) S (store-set X B L R))))

(: store-set-unbranch (-> Store-Set Bool-Set Store-Set))
(define (store-set-unbranch S B)
  (define-values (X B* L R) (store-set-projs S))
  (let ([B  (bool-set-intersect B* B)])
    (if (eq? B* B) S (store-set X B L R))))

(: store-set-unleft (-> Store-Set Store-Set Store-Set))
(define (store-set-unleft S L)
  (define-values (X B L* R) (store-set-projs S))
  (let ([L  (store-set-intersect L* L)])
    (if (eq? L* L) S (store-set X B L R))))

(: store-set-unright (-> Store-Set Store-Set Store-Set))
(define (store-set-unright S R)
  (define-values (X B L R*) (store-set-projs S))
  (let ([R  (store-set-intersect R* R)])
    (if (eq? R* R) S (store-set X B L R))))

;; ===================================================================================================
;; Indexed projections

(: store-set-random-proj (case-> (-> Empty-Store-Set Store-Index Empty-Real-Set)
                                 (-> Nonempty-Store-Set Store-Index Nonextremal-Real-Set)
                                 (-> Store-Set Store-Index Real-Set)))
(define (store-set-random-proj S j)
  (if (empty-store-set? S)
      empty-real-set
      (let loop ([S S] [j  (reverse j)])
        (cond [(stores? S)  unit-interval]
              [(empty? j)  (Nonextremal-Store-Set-random S)]
              [(first j)  (loop (Nonextremal-Store-Set-left  S) (rest j))]
              [else       (loop (Nonextremal-Store-Set-right S) (rest j))]))))

(: store-set-branch-proj (case-> (-> Empty-Store-Set Store-Index Empty-Bool-Set)
                                 (-> Nonempty-Store-Set Store-Index Nonempty-Bool-Set)
                                 (-> Store-Set Store-Index Bool-Set)))
(define (store-set-branch-proj S j)
  (if (empty-store-set? S)
      empty-bool-set
      (let loop ([S S] [j  (reverse j)])
        (cond [(stores? S)  bools]
              [(empty? j)  (Nonextremal-Store-Set-branch S)]
              [(first j)  (loop (Nonextremal-Store-Set-left  S) (rest j))]
              [else       (loop (Nonextremal-Store-Set-right S) (rest j))]))))

;; ===================================================================================================
;; Indexed unprojections

(: store-set-random-unproj (-> Store-Set Store-Index Real-Set Store-Set))
(define (store-set-random-unproj S j X)
  (if (empty-store-set? S)
      empty-store-set
      (let ([X  (real-set-intersect unit-interval X)])
        (if (empty-real-set? X)
            empty-store-set
            (let loop ([S S] [j  (reverse j)])
              (cond [(empty? j)  (store-set-unrandom S X)]
                    [(first j)
                     (define-values (X B L* R) (store-set-projs S))
                     (define L (loop L* (rest j)))
                     (if (eq? L* L) S (store-set X B L R))]
                    [else
                     (define-values (X B L R*) (store-set-projs S))
                     (define R (loop R* (rest j)))
                     (if (eq? R* R) S (store-set X B L R))]))))))

(: store-set-branch-unproj (-> Store-Set Store-Index Bool-Set Store-Set))
(define (store-set-branch-unproj S j B)
  (if (or (empty-store-set? S) (empty-bool-set? B))
      empty-store-set
      (let loop ([S S] [j  (reverse j)])
        (cond [(empty? j)  (store-set-unbranch S B)]
              [(first j)
               (define-values (X B L* R) (store-set-projs S))
               (define L (loop L* (rest j)))
               (if (eq? L* L) S (store-set X B L R))]
              [else
               (define-values (X B L R*) (store-set-projs S))
               (define R (loop R* (rest j)))
               (if (eq? R* R) S (store-set X B L R))]))))

;; ===================================================================================================
;; Intersection

(: store-set-intersect (case-> (-> Store-Set Nonfull-Store-Set Nonfull-Store-Set)
                               (-> Nonfull-Store-Set Store-Set Nonfull-Store-Set)
                               (-> Store-Set Store-Set Store-Set)))
(define (store-set-intersect S1 S2)
  (cond [(stores? S1)  S2]
        [(stores? S2)  S1]
        [(eq? S1 S2)  S1]
        [(or (empty-store-set? S1) (empty-store-set? S2))  empty-store-set]
        [else
         (match-define (Nonextremal-Store-Set X1 B1 L1 R1) S1)
         (match-define (Nonextremal-Store-Set X2 B2 L2 R2) S2)
         (let ([B  (bool-set-intersect B1 B2)])
           (if (empty-bool-set? B)
               empty-store-set
               (let ([X  (real-set-intersect X1 X2)])
                 (if (empty-real-set? X)
                     empty-store-set
                     (let ([L  (store-set-intersect L1 L2)])
                       (if (empty-store-set? L)
                           empty-store-set
                           (let ([R  (store-set-intersect R1 R2)])
                             (if (empty-store-set? R)
                                 empty-store-set
                                 (cond [(and (eq? X X1) (eq? B B1) (eq? L L1) (eq? R R1))  S1]
                                       [(and (eq? X X2) (eq? B B2) (eq? L L2) (eq? R R2))  S2]
                                       [else  (Nonextremal-Store-Set X B L R)])))))))))]))

;; ===================================================================================================
;; Join

(: store-set-join (case-> (-> Store-Set Nonempty-Store-Set Nonempty-Store-Set)
                          (-> Nonempty-Store-Set Store-Set Nonempty-Store-Set)
                          (-> Store-Set Store-Set Store-Set)))
(define (store-set-join S1 S2)
  (cond [(empty-store-set? S1)  S2]
        [(empty-store-set? S2)  S1]
        [(eq? S1 S2)  S1]
        [(or (stores? S1) (stores? S2))  stores]
        [else
         (match-define (Nonextremal-Store-Set X1 B1 L1 R1) S1)
         (match-define (Nonextremal-Store-Set X2 B2 L2 R2) S2)
         (define X (real-set-union X1 X2))
         (define B (bool-set-union B1 B2))
         (define L (store-set-join L1 L2))
         (define R (store-set-join R1 R2))
         (cond [(and (eq? X X1) (eq? B B1) (eq? L L1) (eq? R R1))  S1]
               [(and (eq? X X2) (eq? B B2) (eq? L L2) (eq? R R2))  S2]
               [(reals? X)  (error 'store-set-join "internal error: given ~a and ~a" X1 X2)]
               [else  (Nonextremal-Store-Set X B L R)])]))

;; ===================================================================================================
;; Membership

(: store-set-member? (-> Store-Set Store Boolean))
(define (store-set-member? S s)
  (if (empty-store-set? S)
      #f
      (let loop ([S S] [s s])
        (cond [(stores? S)  #t]
              [else
               (match-define (Nonextremal-Store-Set X B L R) S)
               (and (or (bools? B)
                        (let ([b  (store-branch s)])
                          (if (bottom? b) #f (bool-set-member? B b))))
                    (real-set-member? X (store-random s))
                    (loop L (store-left s))
                    (loop R (store-right s)))]))))

;; ===================================================================================================
;; Subset

(: store-set-subseteq? (-> Store-Set Store-Set Boolean))
(define (store-set-subseteq? S1 S2)
  (cond [(eq? S1 S2)  #t]
        [(empty-store-set? S1)  #t]
        [(empty-store-set? S2)  #f]
        [else
         (let loop ([S1 S1] [S2 S2])
           (cond [(stores? S2)  #t]
                 [(stores? S1)  #f]
                 [else
                  (match-define (Nonextremal-Store-Set X1 B1 L1 R1) S1)
                  (match-define (Nonextremal-Store-Set X2 B2 L2 R2) S2)
                  (and (bool-set-subseteq? B1 B2)
                       (real-set-subseteq? X1 X2)
                       (loop L1 L2)
                       (loop R1 R2))]))]))

;; ===================================================================================================
;; Sample/infimum

(: make-bottom-trace-value (-> Store-Index Bottom))
(define (make-bottom-trace-value j)
  (bottom (delay (format "no branch decision at index ~a" j))))

(: store-set-realize (-> Nonempty-Store-Set Store))
;; Sample each real axis, take infimum of each boolean axis
(define (store-set-realize S)
  (let loop ([S S] [j j0])
    (define-values (X B L R) (store-set-projs S))
    (Store (delay (real-set-sample-point X))
           (delay (if (bools? B) (make-bottom-trace-value j) (trues? B)))
           (delay (loop L (left j)))
           (delay (loop R (right j))))))

;; ===================================================================================================
;; Measurement

(: store-set-random-measure (-> Store-Set Flonum))
(define (store-set-random-measure S)
  (if (empty-store-set? S)
      0.0
      (let loop ([S S])
        (if (stores? S)
            1.0
            (match-let ([(Nonextremal-Store-Set X B L R)  S])
              (* (real-set-measure X) (* (loop L) (loop R))))))))

;; ===================================================================================================

(: store-set-random-list (-> Store-Set (Listof Nonextremal-Real-Set)))
(define (store-set-random-list S)
  (if (empty-store-set? S)
      empty
      (let loop ([S S])
        (if (stores? S)
            empty
            (match-let ([(Nonextremal-Store-Set X B L R)  S])
              (append (if (eq? X unit-interval) empty (list X))
                      (loop L)
                      (loop R)))))))

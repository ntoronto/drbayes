#lang typed/racket/base

(require racket/match
         racket/list
         racket/promise
         "types.rkt"
         "parameters.rkt"
         "bottom.rkt"
         "prob-set.rkt"
         "bool-set.rkt"
         "store-index.rkt"
         "store.rkt"
         "../flonum.rkt"
         "../untyped-utils.rkt"
         "../utils.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Unbounded trees used for program store sets

(struct: Base-Store-Set Base-Bot-Basic () #:transparent)
(define store-set? Base-Store-Set?)

(define-singleton-type Empty-Store-Set Base-Store-Set empty-store-set)
(define-singleton-type Full-Store-Set Base-Store-Set stores)

(struct Plain-Store-Set Base-Store-Set
  ([random : Nonempty-Prob-Set]
   [branch : Nonempty-Bool-Set]
   [left   : Nonempty-Store-Set]
   [right  : Nonempty-Store-Set])
  #:transparent)

(define-type Nonempty-Store-Set (U Plain-Store-Set Full-Store-Set))
(define-type Nonfull-Store-Set (U Plain-Store-Set Empty-Store-Set))
(define-type Store-Set (U Plain-Store-Set Empty-Store-Set Full-Store-Set))

#|
(: plain-store-set-hash (HashTable Nonempty-Prob-Set
                                   (HashTable Nonempty-Bool-Set
                                              (HashTable Nonempty-Store-Set
                                                         (HashTable Nonempty-Store-Set
                                                                    (Weak-Boxof Plain-Store-Set))))))
(define plain-store-set-hash (make-weak-hasheq))
|#

(: plain-store-set (-> Nonempty-Prob-Set
                       Nonempty-Bool-Set
                       Nonempty-Store-Set
                       Nonempty-Store-Set
                       Plain-Store-Set))
(define (plain-store-set X B L R)
  (if set-ensure-unique?
      (error 'plain-store-set "set uniqueness unimplemented")
      #|
      (let* ([h  plain-store-set-hash]
             [h  (hash-ref! h X (inst make-hasheq
                                      Nonempty-Bool-Set
                                      (HashTable Nonempty-Store-Set
                                                 (HashTable Nonempty-Store-Set
                                                            (Weak-Boxof Plain-Store-Set)))))]
             [h  (hash-ref! h B (inst make-weak-hasheq
                                      Nonempty-Store-Set
                                      (HashTable Nonempty-Store-Set
                                                 (Weak-Boxof Plain-Store-Set))))]
             [h  (hash-ref! h L (inst make-weak-hasheq
                                      Nonempty-Store-Set
                                      (Weak-Boxof Plain-Store-Set)))])
        (weak-value-hash-ref! h R (λ () (Plain-Store-Set X B L R))))
|#
      (Plain-Store-Set X B L R)))

(: store-set (case-> (-> Nonempty-Prob-Set
                         Nonempty-Bool-Set
                         Nonempty-Store-Set
                         Nonempty-Store-Set
                         Nonempty-Store-Set)
                     (-> Prob-Set Bool-Set Store-Set Store-Set Store-Set)))
(define (store-set X B L R)
  (cond [(or (empty-prob-set? X) (empty-bool-set? B) (empty-store-set? L) (empty-store-set? R))
         empty-store-set]
        [(and (probs? X) (bools? B) (stores? L) (stores? R))
         stores]
        [else
         (plain-store-set X B L R)]))

;; ===================================================================================================
;; Simple projections

(: store-set-random (case-> (-> Empty-Store-Set Empty-Prob-Set)
                            (-> Nonempty-Store-Set Nonempty-Prob-Set)
                            (-> Store-Set Prob-Set)))
(define (store-set-random S)
  (cond [(empty-store-set? S)  empty-prob-set]
        [(stores? S)  probs]
        [else  (Plain-Store-Set-random S)]))

(: store-set-branch (case-> (-> Empty-Store-Set Empty-Bool-Set)
                            (-> Nonempty-Store-Set Nonempty-Bool-Set)
                            (-> Store-Set Bool-Set)))
(define (store-set-branch S)
  (cond [(empty-store-set? S)  empty-bool-set]
        [(stores? S)  bools]
        [else  (Plain-Store-Set-branch S)]))

(: store-set-left (case-> (-> Empty-Store-Set Empty-Store-Set)
                          (-> Nonempty-Store-Set Nonempty-Store-Set)
                          (-> Store-Set Store-Set)))
(define (store-set-left S)
  (cond [(empty-store-set? S)  empty-store-set]
        [(stores? S)  stores]
        [else  (Plain-Store-Set-left S)]))

(: store-set-right (case-> (-> Empty-Store-Set Empty-Store-Set)
                           (-> Nonempty-Store-Set Nonempty-Store-Set)
                           (-> Store-Set Store-Set)))
(define (store-set-right S)
  (cond [(empty-store-set? S)  empty-store-set]
        [(stores? S)  stores]
        [else  (Plain-Store-Set-right S)]))

(: store-set-projs (case-> (-> Empty-Store-Set (Values Empty-Prob-Set
                                                       Empty-Bool-Set
                                                       Empty-Store-Set
                                                       Empty-Store-Set))
                           (-> Nonempty-Store-Set (Values Nonempty-Prob-Set
                                                          Nonempty-Bool-Set
                                                          Nonempty-Store-Set
                                                          Nonempty-Store-Set))
                           (-> Store-Set (Values Prob-Set
                                                 Bool-Set
                                                 Store-Set
                                                 Store-Set))))
(define (store-set-projs S)
  (cond [(empty-store-set? S)  (values empty-prob-set empty-bool-set empty-store-set empty-store-set)]
        [(stores? S)  (values probs bools stores stores)]
        [else  (values (Plain-Store-Set-random S)
                       (Plain-Store-Set-branch S)
                       (Plain-Store-Set-left   S)
                       (Plain-Store-Set-right  S))]))

;; ===================================================================================================
;; Simple unprojections

(: store-set-unrandom (-> Store-Set Prob-Set Store-Set))
(define (store-set-unrandom S X)
  (define-values (X* B L R) (store-set-projs S))
  (let ([X  (prob-set-intersect X* X)])
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

(: store-set-random-proj (case-> (-> Empty-Store-Set Store-Index Empty-Prob-Set)
                                 (-> Nonempty-Store-Set Store-Index Nonempty-Prob-Set)
                                 (-> Store-Set Store-Index Prob-Set)))
(define (store-set-random-proj S j)
  (if (empty-store-set? S)
      empty-prob-set
      (let loop ([S S] [j  (reverse j)])
        (cond [(stores? S)  probs]
              [(empty? j)  (Plain-Store-Set-random S)]
              [(first j)  (loop (Plain-Store-Set-left  S) (rest j))]
              [else       (loop (Plain-Store-Set-right S) (rest j))]))))

(: store-set-branch-proj (case-> (-> Empty-Store-Set Store-Index Empty-Bool-Set)
                                 (-> Nonempty-Store-Set Store-Index Nonempty-Bool-Set)
                                 (-> Store-Set Store-Index Bool-Set)))
(define (store-set-branch-proj S j)
  (if (empty-store-set? S)
      empty-bool-set
      (let loop ([S S] [j  (reverse j)])
        (cond [(stores? S)  bools]
              [(empty? j)  (Plain-Store-Set-branch S)]
              [(first j)  (loop (Plain-Store-Set-left  S) (rest j))]
              [else       (loop (Plain-Store-Set-right S) (rest j))]))))

;; ===================================================================================================
;; Indexed unprojections

(: store-set-random-unproj (-> Store-Set Store-Index Prob-Set Store-Set))
(define (store-set-random-unproj S j X)
  (if (empty-store-set? S)
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
               (if (eq? R* R) S (store-set X B L R))]))))

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
        [(empty-store-set? S1)  S1]
        [(empty-store-set? S2)  S2]
        [else
         (match-define (Plain-Store-Set X1 B1 L1 R1) S1)
         (match-define (Plain-Store-Set X2 B2 L2 R2) S2)
         (let ([B  (bool-set-intersect B1 B2)])
           (if (empty-bool-set? B)
               empty-store-set
               (let ([X  (prob-set-intersect X1 X2)])
                 (if (empty-prob-set? X)
                     empty-store-set
                     (let ([L  (store-set-intersect L1 L2)])
                       (if (empty-store-set? L)
                           empty-store-set
                           (let ([R  (store-set-intersect R1 R2)])
                             (if (empty-store-set? R)
                                 empty-store-set
                                 (cond [(and (eq? X X1) (eq? B B1) (eq? L L1) (eq? R R1))  S1]
                                       [(and (eq? X X2) (eq? B B2) (eq? L L2) (eq? R R2))  S2]
                                       [else  (plain-store-set X B L R)])))))))))]))

;; ===================================================================================================
;; Join

(: store-set-join (case-> (-> Store-Set Nonempty-Store-Set (Values Nonempty-Store-Set Boolean))
                          (-> Nonempty-Store-Set Store-Set (Values Nonempty-Store-Set Boolean))
                          (-> Store-Set Store-Set (Values Store-Set Boolean))))
(define (store-set-join S1 S2)
  (cond
    [(empty-store-set? S1)  (values S2 #t)]
    [(empty-store-set? S2)  (values S1 #t)]
    [(eq? S1 S2)  (values S1 #t)]
    [(stores? S1)  (values S1 #t)]
    [(stores? S2)  (values S2 #t)]
    [else
     (match-define (Plain-Store-Set X1 B1 L1 R1) S1)
     (match-define (Plain-Store-Set X2 B2 L2 R2) S2)
     (define-values (X x-exact?) (prob-set-join X1 X2))
     (define-values (B b-exact?) (bool-set-join B1 B2))
     (define-values (L l-exact?) (store-set-join L1 L2))
     (define-values (R r-exact?) (store-set-join R1 R2))
     (cond [(and (eq? X X1) (eq? B B1) (eq? L L1) (eq? R R1))  (values S1 #t)]
           [(and (eq? X X2) (eq? B B2) (eq? L L2) (eq? R R2))  (values S2 #t)]
           [else
            (define x-eq? (eq? X1 X2))
            (define b-eq? (eq? B1 B2))
            (define l-eq? (eq? L1 L2))
            (define r-eq? (eq? R1 R2))
            (define exact?
              (or (and x-exact? b-eq? l-eq? r-eq?)
                  (and x-eq? b-exact? l-eq? r-eq?)
                  (and x-eq? b-eq? l-exact? r-eq?)
                  (and x-eq? b-eq? l-eq? r-exact?)))
            (values (plain-store-set X B L R) exact?)])]))

;; ===================================================================================================
;; Membership

(: store-set-member? (-> Store-Set Store Boolean))
(define (store-set-member? S s)
  (if (empty-store-set? S)
      #f
      (let loop ([S S] [s s])
        (cond [(stores? S)  #t]
              [else
               (match-define (Plain-Store-Set X B L R) S)
               (and (or (bools? B)
                        (let ([b  (store-branch s)])
                          (if (bottom? b) #f (bool-set-member? B b))))
                    (let ([x  (store-random s)])
                      (if (bad-prob? x) #f (prob-set-member? X x)))
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
                  (match-define (Plain-Store-Set X1 B1 L1 R1) S1)
                  (match-define (Plain-Store-Set X2 B2 L2 R2) S2)
                  (and (bool-set-subseteq? B1 B2)
                       (prob-set-subseteq? X1 X2)
                       (loop L1 L2)
                       (loop R1 R2))]))]))

;; ===================================================================================================
;; Sample/infimum

(: make-bottom-trace-value (-> Store-Index Bottom))
(define (make-bottom-trace-value j)
  (bottom (delay (format "no branch decision at index ~a" j))))

(: stores-realize (-> Store-Index Store))
(define (stores-realize j)
  (let loop ([j j])
    (Store (delay (prob-random prob-0 prob-1))
           (delay (make-bottom-trace-value j))
           (delay (loop (left j)))
           (delay (loop (right j))))))

(: store-set-realize (-> Nonempty-Store-Set Store))
;; Sample each real axis, take infimum of each boolean axis
(define (store-set-realize S)
  (let loop : Store ([S S] [j j0])
    (cond [(stores? S)  (stores-realize j)]
          [(Plain-Store-Set? S)
           (match-define (Plain-Store-Set X B L R) S)
           (Store (if (probs? X)
                      (delay (prob-random prob-0 prob-1))
                      (prob-set-sample-point X))
                  (if (bools? B)
                      (delay (make-bottom-trace-value j))
                      (trues? B))
                  (loop L (left j))
                  (loop R (right j)))])))

;; ===================================================================================================
;; Measurement

(: make-store-set-random-measure (-> (-> Nonempty-Prob-Set Prob) (-> Prob Prob Prob)
                                     (-> Store-Set Prob)))
(define ((make-store-set-random-measure prob-set-measure prob*) S)
  (if (empty-store-set? S)
      prob-0
      (let loop ([S S])
        (if (stores? S)
            prob-1
            (match-let ([(Plain-Store-Set X B L R)  S])
              (prob* (prob-set-measure X)
                     (prob* (loop L) (loop R))))))))

(define store-set-random-measure
  (make-store-set-random-measure prob-set-measure prob*))

(define store-set-random-measure/rndd
  (make-store-set-random-measure prob-set-measure/rndd prob*/rndd))

(define store-set-random-measure/rndu
  (make-store-set-random-measure prob-set-measure/rndu prob*/rndu))

;; ===================================================================================================

(: store-set-random-list (-> Store-Set (Listof Plain-Prob-Set)))
(define (store-set-random-list S)
  (if (empty-store-set? S)
      empty
      (let loop ([S S])
        (if (stores? S)
            empty
            (match-let ([(Plain-Store-Set X B L R)  S])
              (append (if (probs? X) empty (list X))
                      (loop L)
                      (loop R)))))))

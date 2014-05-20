#lang racket/base

(provide Full-Real-Set reals reals?
         Empty-Real-Set empty-real-set empty-real-set?
         Nonextremal-Interval Nonfull-Interval Nonempty-Interval Interval
         Nonextremal-Interval?
         interval flonum->singleton interval-fields interval?
         positive-interval negative-interval
         zero-interval nonpositive-interval nonnegative-interval
         unit-interval
         interval-intersect
         interval-subseteq?
         interval-member?
         interval-sample-point
         interval-measure
         Nonextremal-Interval-List
         Nonextremal-Real-Set Nonfull-Real-Set Nonempty-Real-Set Real-Set
         interval-list?
         interval-list-elements
         real-set?
         real-set-complement
         real-set-subtract
         real-set-union
         real-set-intersect
         real-set-subseteq?
         real-set-member?
         real-set-sample-point
         real-set-measure
         real-set-map
         )

(module typed-defs typed/racket/base
  (provide (all-defined-out))
  
  (require (for-syntax racket/base)
           racket/match
           racket/flonum
           racket/list
           math/private/utils
           math/flonum
           math/distributions
           "types.rkt"
           "../utils.rkt"
           "../untyped-utils.rkt")
  
  (struct: Base-Real-Set Base-Bot-Basic () #:transparent)
  
  (define real-set? Base-Real-Set?)
  
  (define-singleton-type Full-Real-Set Base-Real-Set reals)
  (define-singleton-type Empty-Real-Set Base-Real-Set empty-real-set)
  
  (: print-interval (Nonextremal-Interval Output-Port (U #t #f 0 1) -> Any))
  (define (print-interval I port mode)
    (match-define (Nonextremal-Interval a b a? b?) I)
    (cond [(and a? b?)  (pretty-print-constructor 'interval (list a b) port mode)]
          [else  (pretty-print-constructor 'interval (list a b a? b?) port mode)]))
  
  (: interval-guard (Flonum Flonum Boolean Boolean Symbol -> (Values Flonum Flonum Boolean Boolean)))
  (define (interval-guard a b a? b? name)
    (cond [(and (= a -inf.0) (= b +inf.0))
           (error name "expected at least one finite endpoint; given ~a ~a ~a ~a" a b a? b?)]
          [(and (or (a . < . b) (and (= a b) a? b?))
                (if (= a -inf.0) (not a?) #t)
                (if (= b +inf.0) (not b?) #t))
           (values a b a? b?)]
          [else
           (error name "expected strictly increasing endpoints; given ~a ~a ~a ~a" a b a? b?)]))
  
  (struct: Nonextremal-Interval Base-Real-Set
    ([min : Flonum] [max : Flonum] [min? : Boolean] [max? : Boolean])
    #:transparent
    #:guard interval-guard
    #:property prop:custom-print-quotable 'never
    #:property prop:custom-write print-interval)
  
  (define-type Nonempty-Interval (U Nonextremal-Interval Full-Real-Set))
  (define-type Nonfull-Interval (U Nonextremal-Interval Empty-Real-Set))
  (define-type Interval (U Nonextremal-Interval Full-Real-Set Empty-Real-Set))
  
  (: interval? (Any -> Boolean : Interval))
  (define (interval? v)
    (or (Nonextremal-Interval? v) (reals? v) (empty-real-set? v)))
  
  (define positive-interval (Nonextremal-Interval 0.0 +inf.0 #f #f))
  (define negative-interval (Nonextremal-Interval -inf.0 -0.0 #f #f))
  (define nonnegative-interval (Nonextremal-Interval 0.0 +inf.0 #t #f))
  (define nonpositive-interval (Nonextremal-Interval -inf.0 -0.0 #f #t))
  (define zero-interval (Nonextremal-Interval -0.0 +0.0 #t #t))
  (define unit-interval (Nonextremal-Interval 0.0 1.0 #t #t))
  
  (: interval (case-> (Flonum Flonum -> Interval)
                           (Flonum Flonum Boolean Boolean -> Interval)))
  (define (interval a b [a? #t] [b? #t])
    (cond [(not (<= -inf.0 a +inf.0))  (raise-argument-error 'interval "Flonum, not NaN" 0 a b)]
          [(not (<= -inf.0 b +inf.0))  (raise-argument-error 'interval "Flonum, not NaN" 1 a b)]
          [(or (> a b) (and (= a b) (not (and a? b?))))  empty-real-set]
          [else
           (let ([a  (if (= a -0.0) +0.0 a)]
                 [b  (if (= b +0.0) -0.0 b)])
             (cond [(fl= a -inf.0)
                    (cond [(fl= b +inf.0)  reals]
                          [(fl= b -inf.0)  empty-real-set]
                          [else  (Nonextremal-Interval -inf.0 b #f b?)])]
                   [(fl= b +inf.0)
                    (cond [(fl= a +inf.0)  empty-real-set]
                          ;[(fl= a -inf.0)  reals]  ; already checked above
                          [else  (Nonextremal-Interval a +inf.0 a? #f)])]
                   [else  (Nonextremal-Interval a b a? b?)]))]))
  
  (: flonum->singleton (Flonum -> Nonextremal-Real-Set))
  (define (flonum->singleton x)
    (cond [(< -inf.0 x +inf.0)  (Nonextremal-Interval x x #t #t)]
          [else  (raise-argument-error 'flonum->singleton "rational Flonum" x)]))
  
  (: interval-fields (Nonempty-Interval -> (Values Flonum Flonum Boolean Boolean)))
  (define (interval-fields I)
    (cond [(reals? I)  (values -inf.0 +inf.0 #f #f)]
          [else  (values (Nonextremal-Interval-min I)
                         (Nonextremal-Interval-max I)
                         (Nonextremal-Interval-min? I)
                         (Nonextremal-Interval-max? I))]))
  
  (: interval-intersect (case-> (Interval Nonfull-Interval -> Nonfull-Interval)
                                (Nonfull-Interval Interval -> Nonfull-Interval)
                                (Interval Interval -> Interval)))
  (define (interval-intersect I1 I2)
    (cond [(empty-real-set? I1)  I1]
          [(empty-real-set? I2)  I2]
          [(reals? I1)  I2]
          [(reals? I2)  I1]
          [else
           (match-define (Nonextremal-Interval a1 b1 a1? b1?) I1)
           (match-define (Nonextremal-Interval a2 b2 a2? b2?) I2)
           (define-values (a a?)
             (cond [(a1 . > . a2)  (values a1 a1?)]
                   [(a1 . < . a2)  (values a2 a2?)]
                   [else           (values a1 (and a1? a2?))]))
           (define-values (b b?)
             (cond [(b1 . > . b2)  (values b2 b2?)]
                   [(b1 . < . b2)  (values b1 b1?)]
                   [else           (values b1 (and b1? b2?))]))
           (cond [(and (fl= a a1) (fl= b b1) (eq? a? a1?) (eq? b? b1?))  I1]
                 [(and (fl= a a2) (fl= b b2) (eq? a? a2?) (eq? b? b2?))  I2]
                 [else
                  (define I (interval a b a? b?))
                  (cond [(reals? I)
                         (raise-result-error 'interval-intersect "Nonfull-Interval" I)]
                        [else  I])])]))
  
  (: interval-subseteq? (Interval Interval -> Boolean))
  (define (interval-subseteq? I1 I2)
    (cond [(empty-real-set? I1)  #t]
          [(empty-real-set? I2)  #f]
          [(reals? I2)   #t]
          [(reals? I1)   #f]
          [else
           (match-define (Nonextremal-Interval a1 b1 a1? b1?) I1)
           (match-define (Nonextremal-Interval a2 b2 a2? b2?) I2)
           (and (or (a1 . > . a2) (and (= a1 a2) (or (not a1?) a2?)))
                (or (b1 . < . b2) (and (= b1 b2) (or (not b1?) b2?))))]))
  
  (: interval-member? (Interval Flonum -> Boolean))
  (define (interval-member? I x)
    (cond [(not (< -inf.0 x +inf.0))  #f]
          [(empty-real-set? I)  #f]
          [(reals? I)   #t]
          [else
           (match-define (Nonextremal-Interval a b a? b?) I)
           (cond [(< a x b)  #t]
                 [(< x a)  #f]
                 [(< b x)  #f]
                 [(and (= x a) a?)  #t]
                 [(and (= x b) b?)  #t]
                 [else  #f])]))
  
  (: interval-sample-point (Nonextremal-Interval -> Flonum))
  (define (interval-sample-point I)
    (match-define (Nonextremal-Interval a b a? b?) I)
    (define m (- b a))
    (define x (+ a (* m (random))))
    (cond [(and (or (not (= x a)) a?) (or (not (= x b)) b?))  x]
          [(and a? b?)  (* 0.5 (+ a b))]
          [a?  a]
          [b?  b]
          [else  (* 0.5 (+ a b))]))
  
  (: interval-measure (Interval -> Flonum))
  (define (interval-measure I)
    (cond [(empty-real-set? I)  0.0]
          [(reals? I)   +inf.0]
          [else  (- (Nonextremal-Interval-max I) (Nonextremal-Interval-min I))]))
  
  ;; =================================================================================================
  ;; Sorted, disjoint interval unions
  
  (: interval-list-valid? ((Listof+2 Nonextremal-Interval) -> Boolean))
  (define (interval-list-valid? Is)
    (let loop ([I1  (first Is)] [I2  (second Is)] [Is  (rest (rest Is))])
      (match-define (Nonextremal-Interval a1 b1 a1? b1?) I1)
      (match-define (Nonextremal-Interval a2 b2 a2? b2?) I2)
      (cond [(not (or (b1 . < . a2) (and (= b1 a2) (not b1?) (not a2?))))  #f]
            [(empty? Is)  #t]
            [else  (loop I2 (first Is) (rest Is))])))
  
  (: interval-list-guard ((Listof+2 Nonextremal-Interval) Symbol -> (Listof+2 Nonextremal-Interval)))
  (define (interval-list-guard Is name)
    (cond [(interval-list-valid? Is)  Is]
          [else  (error name "expected strictly increasing, nonoverlapping intervals; given ~e" Is)]))
  
  (struct: Nonextremal-Interval-List Base-Real-Set
    ([elements : (Listof+2 Nonextremal-Interval)])
    #:transparent
    #:guard interval-list-guard)
  
  (define-type Nonextremal-Real-Set (U Nonextremal-Interval Nonextremal-Interval-List))
  (define-type Nonfull-Real-Set (U Nonextremal-Real-Set Empty-Real-Set))
  (define-type Nonempty-Real-Set (U Nonextremal-Real-Set Full-Real-Set))
  (define-type Real-Set (U Nonextremal-Real-Set Full-Real-Set Empty-Real-Set))
  
  (define-syntax interval-list? (make-rename-transformer #'Nonextremal-Interval-List?))
  (define-syntax interval-list-elements
    (make-rename-transformer #'Nonextremal-Interval-List-elements))
  
  (: min<? (Flonum Boolean Flonum Boolean -> Boolean))
  (define (min<? a1 a1? a2 a2?)
    (or (a1 . < . a2) (and (= a1 a2) a1? (not a2?))))
  
  (: max<? (Flonum Boolean Flonum Boolean -> Boolean))
  (define (max<? b1 b1? b2 b2?)
    (or (b1 . < . b2) (and (= b1 b2) (not b1?) b2?)))
  
  (: nonextremal-real-set->list (Nonextremal-Real-Set -> (Listof+1 Nonextremal-Interval)))
  (define (nonextremal-real-set->list I)
    (if (Nonextremal-Interval? I) (list I) (Nonextremal-Interval-List-elements I)))
  
  (: list->real-set (case-> ((Listof+1 Nonextremal-Interval) -> Nonextremal-Real-Set)
                            ((Listof Nonextremal-Interval) -> Nonfull-Real-Set)))
  (define (list->real-set Is)
    (cond [(empty? Is)  empty-real-set]
          [(empty? (rest Is))  (first Is)]
          [else  (Nonextremal-Interval-List Is)]))
  
  ;; -------------------------------------------------------------------------------------------------
  ;; Complement
  
  (: interval-list-complement (Flonum Flonum Boolean Boolean (Listof Nonextremal-Interval)
                                      -> (Listof Nonextremal-Interval)))
  (define (interval-list-complement a1 b1 a1? b1? I2)
    (cond
      [(empty? I2)  (list (assert (interval a1 b1 a1? b1?) Nonextremal-Interval?))]
      [else
       (match-define (Nonextremal-Interval a2 b2 a2? b2?) (first I2))
       (cond
         [(or (b1 . < . a2) (and (= b1 a2) (or (not b1?) (not a2?))))
          ;; ------
          ;;       ------
          (list (assert (interval a1 b1 a1? b1?) Nonextremal-Interval?))]
         [(or (b2 . < . a1) (and (= b2 a1) (or (not b2?) (not a1?))))
          ;;       ------
          ;; ------
          (interval-list-complement a1 b1 a1? b1? (rest I2))]
         [(min<? a1 a1? a2 a2?)
          (cond [(max<? b2 b2? b1 b1?)
                 ;; ------
                 ;;   --
                 (cons (assert (interval a1 a2 a1? (not a2?)) Nonextremal-Interval?)
                       (interval-list-complement b2 b1 (not b2?) b1? (rest I2)))]
                [else
                 ;; ------           ------
                 ;;    ------   or      ---
                 (list (assert (interval a1 a2 a1? (not a2?)) Nonextremal-Interval?))])]
         [else
          (cond [(max<? b2 b2? b1 b1?)
                 ;;    ------        ------
                 ;; ------      or   ---
                 (interval-list-complement b2 b1 (not b2?) b1? (rest I2))]
                [else
                 ;;   --             ---        ---           ------
                 ;; ------   or   ------   or   ------   or   ------
                 empty])])]))
  
  (: real-set-complement (case-> (Nonextremal-Real-Set -> Nonextremal-Real-Set)
                                 (Nonfull-Real-Set -> Nonempty-Real-Set)
                                 (Nonempty-Real-Set -> Nonfull-Real-Set)
                                 (Real-Set -> Real-Set)))
  (define (real-set-complement I)
    (cond [(empty-real-set? I)  reals]
          [(reals? I)  empty-real-set]
          [else
           (list->real-set
            (assert (interval-list-complement -inf.0 +inf.0 #f #f
                                              (nonextremal-real-set->list I))
                    pair?))]))
  
  ;; -------------------------------------------------------------------------------------------------
  ;; Subtraction
  
  (: interval-list-subtract ((Listof Nonextremal-Interval) (Listof Nonextremal-Interval)
                                                           -> (Listof Nonextremal-Interval)))
  (define (interval-list-subtract I1 I2)
    (cond
      [(or (empty? I1) (empty? I2))  I1]
      [else
       (match-define (Nonextremal-Interval a1 b1 a1? b1?) (first I1))
       (match-define (Nonextremal-Interval a2 b2 a2? b2?) (first I2))
       (cond
         [(or (b1 . < . a2) (and (= b1 a2) (or (not b1?) (not a2?))))
          ;; ------
          ;;       ------
          (cons (first I1) (interval-list-subtract (rest I1) I2))]
         [(or (b2 . < . a1) (and (= b2 a1) (or (not b2?) (not a1?))))
          ;;       ------
          ;; ------
          (interval-list-subtract I1 (rest I2))]
         [(min<? a1 a1? a2 a2?)
          (cond [(max<? b2 b2? b1 b1?)
                 ;; ------
                 ;;   --
                 (define I3 (assert (interval a1 a2 a1? (not a2?)) Nonextremal-Interval?))
                 (define I4 (assert (interval b2 b1 (not b2?) b1?) Nonextremal-Interval?))
                 (cons I3 (interval-list-subtract (cons I4 (rest I1)) (rest I2)))]
                [else
                 ;; ------           ------
                 ;;    ------   or      ---
                 (define I3 (assert (interval a1 a2 a1? (not a2?)) Nonextremal-Interval?))
                 (cons I3 (interval-list-subtract (rest I1) I2))])]
         [else
          (cond [(max<? b2 b2? b1 b1?)
                 ;;    ------        ------
                 ;; ------      or   ---
                 (define I4 (assert (interval b2 b1 (not b2?) b1?) Nonextremal-Interval?))
                 (interval-list-subtract (cons I4 (rest I1)) (rest I2))]
                [else
                 ;;   --             ---        ---           ------
                 ;; ------   or   ------   or   ------   or   ------
                 (interval-list-subtract (rest I1) I2)])])]))
  
  (: real-set-subtract (case-> (Full-Real-Set Nonextremal-Real-Set -> Nonextremal-Real-Set)
                               (Full-Real-Set Nonfull-Real-Set -> Nonempty-Real-Set)
                               (Real-Set Nonempty-Real-Set -> Nonfull-Real-Set)
                               (Real-Set Real-Set -> Real-Set)))
  (define (real-set-subtract I1 I2)
    (cond [(empty-real-set? I2)  I1]
          [(empty-real-set? I1)  empty-real-set]
          [(reals? I2)  empty-real-set]
          [(reals? I1)  (real-set-complement I2)]
          [else
           (list->real-set (interval-list-subtract (nonextremal-real-set->list I1)
                                                   (nonextremal-real-set->list I2)))]))
  
  ;; -------------------------------------------------------------------------------------------------
  ;; Union
  
  (: interval-list-union ((Listof Nonextremal-Interval) (Listof Nonextremal-Interval)
                                                        -> (U Full-Real-Set
                                                              (Listof Nonextremal-Interval))))
  (define (interval-list-union I1 I2)
    (cond
      [(empty? I1)  I2]
      [(empty? I2)  I1]
      [else
       (match-define (Nonextremal-Interval a1 b1 a1? b1?) (first I1))
       (match-define (Nonextremal-Interval a2 b2 a2? b2?) (first I2))
       (cond
         [(or (b1 . < . a2) (and (= b1 a2) (not b1?) (not a2?)))
          ;; ------
          ;;        ------
          (let ([I  (interval-list-union (rest I1) I2)])
            (if (reals? I) I (cons (first I1) I)))]
         [(or (b2 . < . a1) (and (= b2 a1) (not b2?) (not a1?)))
          ;;        ------
          ;; ------
          (let ([I  (interval-list-union I1 (rest I2))])
            (if (reals? I) I (cons (first I2) I)))]
         [(min<? a1 a1? a2 a2?)
          (cond [(max<? b2 b2? b1 b1?)
                 ;; ------
                 ;;   --
                 (interval-list-union I1 (rest I2))]
                [else
                 ;; ------           ------
                 ;;    ------   or      ---
                 (define I (interval a1 b2 a1? b2?))
                 (cond [(reals? I)  I]
                       [else  (let ([I  (assert I Nonextremal-Interval?)])
                                (interval-list-union (rest I1) (cons I (rest I2))))])])]
         [else
          (cond [(max<? b2 b2? b1 b1?)
                 ;;    ------        ------
                 ;; ------      or   ---
                 (define I (interval a2 b1 a2? b1?))
                 (cond [(reals? I)  I]
                       [else  (let ([I  (assert I Nonextremal-Interval?)])
                                (interval-list-union (cons I (rest I1)) (rest I2)))])]
                [else
                 ;;   --             ---        ---           ------
                 ;; ------   or   ------   or   ------   or   ------
                 (interval-list-union (rest I1) I2)])])]))
  
  (: real-set-union (case-> (Real-Set Nonempty-Real-Set -> Nonempty-Real-Set)
                            (Nonempty-Real-Set Real-Set -> Nonempty-Real-Set)
                            (Real-Set Real-Set -> Real-Set)))
  (define (real-set-union I1 I2)
    (cond [(empty-real-set? I1)  I2]
          [(empty-real-set? I2)  I1]
          [(reals? I1)   I1]
          [(reals? I2)   I2]
          [else
           (define I (interval-list-union (nonextremal-real-set->list I1)
                                          (nonextremal-real-set->list I2)))
           (if (reals? I) I (list->real-set (assert I pair?)))]))
  
  ;; -------------------------------------------------------------------------------------------------
  ;; Intersection
  
  (: interval-list-intersect ((Listof Nonextremal-Interval) (Listof Nonextremal-Interval)
                                                            -> (Listof Nonextremal-Interval)))
  (define (interval-list-intersect I1 I2)
    (cond
      [(empty? I1)  I1]
      [(empty? I2)  I2]
      [else
       (match-define (Nonextremal-Interval a1 b1 a1? b1?) (first I1))
       (match-define (Nonextremal-Interval a2 b2 a2? b2?) (first I2))
       (cond
         [(or (b1 . < . a2) (and (= b1 a2) (or (not b1?) (not a2?))))
          ;; ------
          ;;       ------
          (interval-list-intersect (rest I1) I2)]
         [(or (b2 . < . a1) (and (= b2 a1) (or (not b2?) (not a1?))))
          ;;       ------
          ;; ------
          (interval-list-intersect I1 (rest I2))]
         [(min<? a1 a1? a2 a2?)
          (cond [(max<? b2 b2? b1 b1?)
                 ;; ------
                 ;;   --
                 (cons (first I2) (interval-list-intersect I1 (rest I2)))]
                [else
                 ;; ------           ------
                 ;;    ------   or      ---
                 (define I (assert (interval a2 b1 a2? b1?) Nonextremal-Interval?))
                 (cons I (interval-list-intersect (rest I1) I2))])]
         [else
          (cond [(max<? b2 b2? b1 b1?)
                 ;;    ------        ------
                 ;; ------      or   ---
                 (define I (assert (interval a1 b2 a1? b2?) Nonextremal-Interval?))
                 (cons I (interval-list-intersect I1 (rest I2)))]
                [else
                 ;;   --             ---        ---           ------
                 ;; ------   or   ------   or   ------   or   ------
                 (cons (first I1) (interval-list-intersect (rest I1) I2))])])]))
  
  (: real-set-intersect (case-> (Real-Set Nonfull-Real-Set -> Nonfull-Real-Set)
                                (Nonfull-Real-Set Real-Set -> Nonfull-Real-Set)
                                (Real-Set Real-Set -> Real-Set)))
  (define (real-set-intersect I1 I2)
    (cond [(empty-real-set? I1)  I1]
          [(empty-real-set? I2)  I2]
          [(reals? I1)  I2]
          [(reals? I2)  I1]
          [(and (Nonextremal-Interval? I1) (Nonextremal-Interval? I2))  (interval-intersect I1 I2)]
          [else  (list->real-set
                  (interval-list-intersect (nonextremal-real-set->list I1)
                                           (nonextremal-real-set->list I2)))]))
  
  ;; -------------------------------------------------------------------------------------------------
  ;; Subset
  
  (: interval-list-subseteq? ((Listof Nonextremal-Interval) (Listof Nonextremal-Interval) -> Boolean))
  (define (interval-list-subseteq? I1 I2)
    (cond [(empty? I1)  #t]
          [(empty? I2)  #f]
          [else
           (match-define (Nonextremal-Interval a1 b1 a1? b1?) (first I1))
           (match-define (Nonextremal-Interval a2 b2 a2? b2?) (first I2))
           (cond
             [(or (b1 . < . a2) (and (= b1 a2) (or (not b1?) (not a2?))))
              ;; ------
              ;;       ------
              #f]
             [(or (b2 . < . a1) (and (= b2 a1) (or (not b2?) (not a1?))))
              ;;       ------
              ;; ------
              (interval-list-subseteq? I1 (rest I2))]
             [(min<? a1 a1? a2 a2?)
              ;; ------        ------           ------
              ;;   --     or      ------   or      ---
              #f]
             [(max<? b2 b2? b1 b1?)
              ;;    ------        ------
              ;; ------      or   ---
              #f]
             [else
              ;;   --             ---        ---           ------
              ;; ------   or   ------   or   ------   or   ------
              (interval-list-subseteq? (rest I1) I2)])]))
  
  (: real-set-subseteq? (Real-Set Real-Set -> Boolean))
  (define (real-set-subseteq? I1 I2)
    (cond [(empty-real-set? I1)  #t]
          [(empty-real-set? I2)  #f]
          [(reals? I2)  #t]
          [(reals? I1)  #f]
          [(and (Nonextremal-Interval? I1) (Nonextremal-Interval? I2))  (interval-subseteq? I1 I2)]
          [else  (interval-list-subseteq? (nonextremal-real-set->list I1)
                                          (nonextremal-real-set->list I2))]))
  
  ;; -------------------------------------------------------------------------------------------------
  ;; Other ops
  
  (: real-set-member? (Real-Set Flonum -> Boolean))
  (define (real-set-member? I x)
    (cond [(not (< -inf.0 x +inf.0))  #f]
          [(empty-real-set? I)  #f]
          [(reals? I)  #t]
          [(Nonextremal-Interval? I)   (interval-member? I x)]
          [else  (ormap (λ: ([I : Interval]) (interval-member? I x))
                        (Nonextremal-Interval-List-elements I))]))
  
  (: real-set-sample-point (Nonextremal-Real-Set -> Flonum))
  (define (real-set-sample-point I)
    (cond [(Nonextremal-Interval? I)  (interval-sample-point I)]
          [else
           (define Is (Nonextremal-Interval-List-elements I))
           (define i (sample-index (normalize-probs/+2 (map/+2 interval-measure Is))))
           (interval-sample-point (list-ref Is i))]))
  
  (: real-set-measure (Real-Set -> Flonum))
  (define (real-set-measure I)
    (cond [(empty-real-set? I)  0.0]
          [(reals? I)  +inf.0]
          [(Nonextremal-Interval? I)   (interval-measure I)]
          [else  (flsum (map interval-measure (Nonextremal-Interval-List-elements I)))]))
  
  (: real-set-map ((Nonempty-Interval -> Real-Set) Real-Set -> Real-Set))
  (define (real-set-map f I)
    (cond [(empty-real-set? I)  empty-real-set]
          [(reals? I)  (f I)]
          [else  (define Is (map f (nonextremal-real-set->list I)))
                 (foldr real-set-union empty-real-set Is)]))
  
  )  ; module type

(require (submod "." typed-defs))

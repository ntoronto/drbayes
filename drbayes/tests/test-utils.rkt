#lang typed/racket/base

(require racket/list
         racket/match
         (only-in typed/rackunit check-true)
         plot
         plot/utils
         math/flonum
         math/base
         math/statistics
         drbayes/private/set
         drbayes/private/flonum)

(provide (all-defined-out)
         profile-thunk
         profile-expr)

;; ===================================================================================================
;; Profiling

(require/typed
 profile
 [profile-thunk  ((-> Any) -> Void)])

(define: b : Boolean #f)

(define-syntax-rule (profile-expr e . args)
  (let* ([thnk  (λ () e)]
         [val  (if b (thnk) #f)])
    (profile-thunk (λ () (set! val (thnk))) . args)
    (assert val (λ: ([x : Any]) x))))

;; ===================================================================================================
;; Plotting utils

(: prob-set->ivls (Nonempty-Prob-Set -> (Listof ivl)))
(define (prob-set->ivls I)
  (cond [(or (probs? I) (Plain-Prob-Interval? I))
         (define-values (a b a? b?) (prob-interval-fields I))
         (list (ivl (prob->flonum a) (prob->flonum b)))]
        [else
         (append* (map prob-set->ivls (Prob-Interval-List-elements I)))]))

(: maybe-pad-list (All (A) ((Listof A) Integer (-> A) -> (Listof A))))
(define (maybe-pad-list lst n thnk)
  (append lst (build-list (max 0 (- n (length lst))) (λ (_) (thnk)))))

(: cons-product (All (A B) ((Listof A) (Listof B) -> (Listof (Pair A B)))))
(define (cons-product as bs)
  (let a-loop ([as as])
    (cond [(empty? as)  empty]
          [else
           (define a (first as))
           (let b-loop ([bs bs])
             (cond [(empty? bs)  (a-loop (rest as))]
                   [else  (cons (cons a (first bs)) (b-loop (rest bs)))]))])))

(: list-product (All (A) ((Listof (Listof A)) -> (Listof (Listof A)))))
(define (list-product xss)
  (cond [(empty? xss)  (list empty)]
        [else  (cons-product (first xss) (list-product (rest xss)))]))

(: store-set->plot-rects (Nonempty-Store-Set -> (Listof (Vectorof ivl))))
(define (store-set->plot-rects S)
  (map (λ ([lst : (Listof ivl)])
         (define n (length lst))
         (list->vector
          (cond [(< n 3)  (append lst (make-list (- 3 n) (ivl 0 1)))]
                [(> n 3)  (take lst 3)]
                [else  lst])))
       (list-product (map prob-set->ivls (store-set-random-list S)))))

(: store->point (-> Store (Listof Flonum)))
(define (store->point s)
  (define lst (map (λ ([p : (U Prob Bad-Prob)])
                     (if (prob? p) (prob->flonum p) +nan.0))
                   (store-random-list s)))
  (define n (length lst))
  (cond [(< n 3)  (append lst (build-list (- 3 n) (λ (_) (random))))]
        [(> n 3)  (take lst 3)]
        [else  lst]))

(: value->point (-> Value (Listof Flonum)))
(define (value->point v)
  (cond [(flonum? v)  (list v)]
        [(prob? v)  (list (prob->flonum v))]
        [(boolean? v)  (list (if v 1.0 0.0))]
        [(pair? v)  (append (value->point (car v))
                            (value->point (cdr v)))]
        [(null? v)  (list)]
        [(tagged-value? v)  (value->point (tagged-value-value v))]
        [(store? v)  (store->point v)]))

;; ===================================================================================================
;; Rendering store sets

(: plot-ivl-size (-> ivl Flonum))
(define (plot-ivl-size i)
  (match-define (ivl a b) i)
  (fl (- (if b b 1.0) (if a a 0.0))))

(: plot-rect-size (-> Boolean (-> (Vectorof ivl) Flonum)))
(define ((plot-rect-size 3d?) r)
  (define s (fl (apply * (map plot-ivl-size (vector->list r)))))
  (if 3d? (sqr (flexpt s #i1/3)) s))

(: alpha-expt (-> Flonum Flonum Flonum))
(define (alpha-expt a n)
  (- 1.0 (flexpt (- 1.0 a) n)))

(: alpha-clamp (-> Flonum Positive-Flonum))
(define (alpha-clamp a)
  (max #i1/256 (min 1.0 a)))

(: store-sets->plot-rects (-> (Listof Nonempty-Store-Set) Nonnegative-Flonum Boolean
                             (Values (Listof (Vectorof ivl))
                                     (Listof Nonnegative-Flonum))))
(define (store-sets->plot-rects Ss alpha 3d?)
  (define rects (append* (map store-set->plot-rects Ss)))
  (define n (length rects))
  (define join-coverage ((plot-rect-size 3d?) (apply rect-join rects)))
  (define sum-coverage (flsum (map (plot-rect-size 3d?) rects)))
  (define overdraw (/ sum-coverage join-coverage))
  (define target-alpha (alpha-expt alpha (/ overdraw)))
  (let-values ([(rects counts)  (count-samples rects)])
    (define alphas
      (for/list : (Listof Nonnegative-Flonum) ([rect  (in-list rects)]
                                               [count  (in-list counts)])
        (define count-prop (/ count n))
        (define size-prop (/ ((plot-rect-size 3d?) rect) sum-coverage))
        (alpha-clamp (alpha-expt target-alpha (fl (/ count-prop size-prop))))))
    (values rects alphas)))

(: store-sets-renderer3d (-> (Listof Nonempty-Store-Set) Nonnegative-Flonum (Listof renderer3d)))
(define (store-sets-renderer3d Ss alpha)
  (define-values (rects alphas) (store-sets->plot-rects Ss alpha #t))
  (for/list ([rect  (in-list rects)]
             [alpha  (in-list alphas)])
    (rectangles3d (list rect) #:color 3 #:line-color 3 #:line-width 0.25 #:alpha alpha)))

(: store-sets-renderer2d (-> (Listof Nonempty-Store-Set) Nonnegative-Flonum (Listof renderer2d)))
(define (store-sets-renderer2d Ss alpha)
  (define-values (rects alphas) (store-sets->plot-rects Ss alpha #f))
  (for/list ([rect  (in-list rects)]
             [alpha  (in-list alphas)])
    (rectangles (list rect) #:color 3 #:line-color 3 #:line-width 0.25 #:alpha alpha)))

;; ===================================================================================================
;; Rendering weighted points, stores and values

(: values->plot-points (-> (Listof Value) (Listof Nonnegative-Flonum) Nonnegative-Real
                           (Values (Listof (Listof (Listof Flonum)))
                                   (Listof Positive-Integer))))
(define (values->plot-points vs ws size)
  (cond
    [(or (empty? vs) (empty? ws))  (values empty empty)]
    [else
     (define ps (map value->point vs))
     (define μ (mean ws))
     (define sizes (map (λ ([w : Nonnegative-Flonum])
                          (max 1 (exact-ceiling (* size (flsqrt (/ w μ))))))
                        ws))
     (define unique-sizes (sort (remove-duplicates sizes) <))
     (define pss
       (for/list : (Listof (Listof (Listof Flonum))) ([size*  (in-list unique-sizes)])
         (for/list : (Listof (Listof Flonum)) ([p  (in-list ps)]
                                               [size  (in-list sizes)]
                                               #:when (= size size*))
           p)))
     (values pss unique-sizes)]))

(: values-renderer2d (-> (Listof Value) (Listof Nonnegative-Flonum)
                         Nonnegative-Real Nonnegative-Real
                         (Listof renderer2d)))
(define (values-renderer2d vs ws size alpha)
  (define-values (pss sizes) (values->plot-points vs ws size))
  (for/list ([ps  (in-list pss)]
             [size  (in-list sizes)])
    (points ps #:sym 'dot #:size size #:alpha alpha)))

(: values-renderer3d (-> (Listof Value) (Listof Nonnegative-Flonum)
                         Nonnegative-Real Nonnegative-Real
                         (Listof renderer3d)))
(define (values-renderer3d vs ws size alpha)
  (define-values (pss sizes) (values->plot-points vs ws size))
  (for/list ([ps  (in-list pss)]
             [size  (in-list sizes)])
    (points3d ps #:sym 'dot #:size size #:alpha alpha)))

;; ===================================================================================================
;; Testing utils

(: random-element (All (A) ((Listof A) -> A)))
(define (random-element xs)
  (list-ref xs (random (length xs))))

;; Using this is about 1000x faster than using `check-true' directly, mostly because it doesn't have
;; to construct the message unless there's a failure
(define-syntax-rule (check-prop expr msg)
  (if expr (void) (check-true expr msg)))

(define-syntax-rule (implies a b) (or (not a) b))

(: flip (All (A B C) ((A B -> C) -> (B A -> C))))
(define ((flip f) x y) (f y x))

(: join->union (All (T) (-> Symbol (-> T T (Values T Boolean)) (-> T T T))))
(define ((join->union name join) A B)
  (define-values (C C-exact?) (join A B))
  (unless C-exact?
    (error name "expected result to be flagged as exact union of ~e and ~e; got ~e" A B C))
  C)

(: intersect->meet (All (T) (-> (-> T T T) (-> T T (Values T #t)))))
(define ((intersect->meet intersect) A B)
  (values (intersect A B) #t))

#lang typed/racket

(require plot/typed
         images/flomap
         racket/flonum
         "../main.rkt"
         "test-utils.rkt")

(printf "starting...~n")

(define/drbayes (vec+ lst1 lst2)
  (list (+ (list-ref lst1 0) (list-ref lst2 0))
        (+ (list-ref lst1 1) (list-ref lst2 1))
        (+ (list-ref lst1 2) (list-ref lst2 2))))

(define/drbayes (vec- lst1 lst2)
  (list (- (list-ref lst1 0) (list-ref lst2 0))
        (- (list-ref lst1 1) (list-ref lst2 1))
        (- (list-ref lst1 2) (list-ref lst2 2))))

(define/drbayes (vec-neg lst)
  (list (- (list-ref lst 0))
        (- (list-ref lst 1))
        (- (list-ref lst 2))))

(define/drbayes (vec-mag^2 lst)
  (+ (+ (sqr (list-ref lst 0))
        (sqr (list-ref lst 1)))
     (sqr (list-ref lst 2))))

(define/drbayes (vec-dot lst1 lst2)
  (+ (+ (* (list-ref lst1 0) (list-ref lst2 0))
        (* (list-ref lst1 1) (list-ref lst2 1)))
     (* (list-ref lst1 2) (list-ref lst2 2))))

(define/drbayes (vec-norm lst)
  (let ([z  (sqrt (vec-mag^2 lst))])
    (list (/ (list-ref lst 0) z)
          (/ (list-ref lst 1) z)
          (/ (list-ref lst 2) z))))

(define/drbayes (vec-scale lst s)
  (list (* (list-ref lst 0) s)
        (* (list-ref lst 1) s)
        (* (list-ref lst 2) s)))

(define/drbayes (min a b)
  (strict-if (a . < . b) a b))

(define/drbayes (max a b)
  (strict-if (a . > . b) a b))

#;; This implementation results in awful image approximations when the denominator in the
;; normalization may be zero
(define/drbayes (uniform-vec)
  (vec-norm (list (random-std-normal) (random-std-normal) (random-std-normal))))

;; Unnormalized uniform direction
(define/drbayes (uniform-vec)
  (list (max -2 (min 2 (random-std-normal)))
        (max -2 (min 2 (random-std-normal)))
        (max -2 (min 2 (random-std-normal)))))

#;; Polar coordinate sampling uses fewer random variables than above, and doesn't do division
(define/drbayes (uniform-vec)
  (let ([θ  (uniform (const (- pi)) (const pi))]
        [φ  (* 2 (asin (sqrt (random))))])
    (let ([sin-φ  (partial-sin φ)])
      (list (* sin-φ (partial-cos θ))
            (* sin-φ (partial-sin θ))
            (partial-cos φ)))))

(define/drbayes (uniform-vec/dir n)
  (let ([v  (uniform-vec)])
    (strict-if (positive? (vec-dot n v)) v (vec-neg v))))

(struct/drbayes collision (time point normal))

(define/drbayes (closer-collision c1 c2)
  (strict-if (and (collision? c1) (collision? c2))
      (strict-if ((collision-time c2) . < . (collision-time c1)) c2 c1)
      (strict-if (collision? c1) c1 c2)))

(define/drbayes (ray-reflect d n)
  (vec- d (vec-scale n (* 2.0 (vec-dot d n)))))

(define/drbayes (ray-sphere-intersect p v c r)
  (let* ([dp  (vec- c p)]
         [-b/2  (vec-dot dp v)]
         [disc  (- (sqr -b/2) (- (vec-mag^2 dp) (sqr r)))])
    (if (positive? disc)
        (let ([t  (- -b/2 (sqrt disc))])
          (if (positive? t)
              (let* ([p1  (vec+ p (vec-scale v t))]
                     [n   (vec-scale (vec- p1 c) (/ r))])
                (collision t p1 n))
              #f))
        #f)))

(define/drbayes (ray-plane-intersect p0 v n d)
  (let ([denom  (- (vec-dot v n))])
    (strict-if (positive? denom)
        (let ([t  (/ (+ d (vec-dot p0 n)) denom)])
          (strict-if (positive? t)
              (collision t (vec+ p0 (vec-scale v t)) n)
              #f))
        #f)))

(define plane1-n (list 0.0 1.0 0.0))
(define plane1-d 0.0)
(define plane2-n (list 0.0 -1.0 0.0))
(define plane2-d 1.0)
(define plane3-n (list 1.0 0.0 0.0))
(define plane3-d 0.0)
(define plane4-n (list -1.0 0.0 0.0))
(define plane4-d 1.0)
(define plane5-n (list 0.0 0.0 1.0))
(define plane5-d 0.0)
(define plane6-n (list 0.0 0.0 -1.0))
(define plane6-d 1.0)

(define sphere0-pc (list 0.4 0.6 0.4))
(define sphere0-r 0.25)

#;; Cast one unit in direction d
(define/drbayes (trace-light ps d)
  (cons (vec+ d (car ps)) ps))

#;; Intersection test against plane
(define/drbayes (trace-light ps d)
  (let* ([p0  (car ps)]
         [c   (ray-plane-intersect p0 d (const plane1-n) (const plane1-d))])
    (if (collision? c)
        (cons (collision-point c) ps)
        ps)))

#;; Intersection test against sphere
(define/drbayes (trace-light ps d)
  (let* ([p0  (car ps)]
         [c   (ray-sphere-intersect p0 d (const sphere0-pc) (const sphere0-r))])
    (if (collision? c)
        (cons (collision-point c) ps)
        ps)))

(define/drbayes (box-intersect p0 d)
  (closer-collision
   (closer-collision
     (ray-plane-intersect p0 d (const plane3-n) (const plane3-d))
     (ray-plane-intersect p0 d (const plane4-n) (const plane4-d)))
   (closer-collision
    (closer-collision
     (ray-plane-intersect p0 d (const plane1-n) (const plane1-d))
     (ray-plane-intersect p0 d (const plane2-n) (const plane2-d)))
    (closer-collision
     (ray-plane-intersect p0 d (const plane5-n) (const plane5-d))
     (ray-plane-intersect p0 d (const plane6-n) (const plane6-d))))))

(define/drbayes (trace-light ps d)
  (let* ([p0  (car ps)]
         [c   (box-intersect p0 d)])
    (if (collision? c)
        ;(cons (collision-point c) ps)
        (let* ([p0  (collision-point c)]
               [n  (collision-normal c)]
               [d  (uniform-vec/dir n)]
               [ps  (cons p0 ps)]
               [c   (ray-plane-intersect p0 d (const plane1-n) (const plane1-d))])
          (strict-if (collision? c)
              (cons (collision-point c) ps)
              ps))
        ps)))

#;
(define/drbayes (trace-light ps d)
  (let* ([p0  (car ps)]
         [c   (closer-collision
               (ray-sphere-intersect p0 d (const sphere0-pc) (const sphere0-r))
               (ray-plane-intersect p0 d (const plane1-n) (const plane1-d)))])
    (strict-if (collision? c)
               (let ([d  (collision-data c)])
                 (if (null? d)
                     (cons (collision-point c) ps)
                     (trace-light (cons (collision-point c) ps) d)))
               ps)))

(define p0 (list 0.9 0.25 0.9))

(define/drbayes (start-p)
  (const p0))

(interval-max-splits 0)
;(interval-min-length (expt 0.5 5.0))

(define n 200)

(define/drbayes e
  (trace-light (list (start-p)) (uniform-vec)))

(define H
  ;(set-list reals reals reals)
  (set-list (real-set 0.49 0.51)
            (real-set -0.001 0.001)
            (real-set 0.49 0.51)))

(define B
  ;universe
  ;(set-list* H universe universe)
  (set-list* H
             universe
             universe
             universe))

#|
(match-define (rand-expression-meaning idxs f-fwd f-comp) (run-rand-expression (->rand f-expr)))

(define refine
  (cond [(empty-set? B)  (error 'refine "B is empty-set")]
        [else  (preimage-refiner f-comp B)]))
|#

(define-values (pss ws)
  (let ()
    (define pws
      (time
       ;profile-expr
       #;
       (let ()
         (define ps (build-list n (λ: ([_ : Index]) (drbayes-run e))))
         (define ws (build-list n (λ: ([_ : Index]) 1.0)))
         (map (inst cons Value Flonum) ps ws))
       (let ()
         (define-values (ps ws) (drbayes-sample e n B))
         (map (inst cons Value Flonum) ps ws))))
    (values (cast (map (inst car Value Flonum) pws)
                  (Listof (Listof (List Flonum Flonum Flonum))))
            (map (inst cdr Value Flonum) pws))))

(printf "search stats:~n")
(get-search-stats)
(newline)

#|
(printf "cache stats:~n")
(get-cache-stats)
(newline)
|#

(printf "(length pss) = ~v~n" (length pss))

;(plot-background '(0.1 0.1 0.1))
;(plot-foreground 'white)

(plot3d (ann (map (λ: ([ps : (Listof (Listof Flonum))])
                    (lines3d ps))
                  (if ((length pss) . > . 500) (take pss 500) pss))
             (Listof renderer3d))
        #:x-min -0.1 #:x-max 1.1
        #:y-min -0.1 #:y-max 1.1
        #:z-min -0.1 #:z-max 1.1)

(define plane1-ccd-d 0.5)

(define xys
  (map (λ: ([ps : (Listof (Listof Flonum))])
         (define p0 (second ps))
         (define p1 (first ps))
         (define d (vec- p1 p0))
         (define c (ray-plane-intersect p1 d plane1-n plane1-ccd-d))
         (define p (cast (collision-point c) (Listof Flonum)))
         (list (- 1.0 (first p)) (- 1.0 (third p))))
       pss))

(define width 256)
(define height 256)
(define fm (make-flomap 1 width height))
(define vs (flomap-values fm))

(for: ([xy  (in-list xys)]
       [w  (in-list ws)])
  (match-define (list orig-x orig-y) xy)
  (define x (exact-round (* width orig-x)))
  (define y (exact-round (* height (- 1.0 orig-y))))
  (when (and (<= 0 x) (< x width)
             (<= 0 y) (< y height))
    (define i (coords->index 1 width 0 x y))
    (flvector-set! vs i (+ w (flvector-ref vs i)))))

(flomap->bitmap (flomap-normalize (flomap-blur fm 1.0)))

(define fm2 (make-flomap 1 width height))
(define vs2 (flomap-values fm2))
(for: ([xy  (in-list xys)])
  (match-define (list orig-x orig-y) xy)
  (define x (exact-round (* width orig-x)))
  (define y (exact-round (* height (- 1.0 orig-y))))
  (when (and (<= 0 x) (< x width)
             (<= 0 y) (< y height))
    (define i (coords->index 1 width 0 x y))
    (flvector-set! vs2 i (+ 1.0 (flvector-ref vs2 i)))))

(flomap->bitmap (flomap-normalize (flomap-blur fm2 1.0)))

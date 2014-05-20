#lang typed/racket

(require plot/typed
         math/distributions
         math/statistics
         math/flonum
         "../../main.rkt"
         "../test-utils.rkt"
         "../profile.rkt"
         "../normal-normal.rkt")

(printf "starting...~n~n")

(error-print-width 1024)

(interval-max-splits 5)
(define n 1000)

#;
(begin
  (define/drbayes e
    (let* ([x  (uniform -1 1)]
           [y  (/ x 5)])
      (list x y)))
  
  (define B (set-list reals (real-set 0.0 0.0))))

#;
(begin
  (interval-max-splits 0)
  (define/drbayes e
    (let* ([x  (uniform -1 1)]
           [y  (/ (normal x 0.1) 5)])
      (list x y)))
  
  (define B (set-list reals (real-set -0.01 0.01))))

#;; Test: Normal-Normal model
;; Preimage should be a banana shape
(begin
  (interval-max-splits 2)
  ;(interval-min-length (expt 0.5 1.0))
  
  (define/drbayes e
    (let* ([x  (normal 0 1)]
           [y  (normal x 1)])
      (list x y)))
  
  (define B (set-list reals (real-set 0.9 1.1)))
  
  (normal-normal/lw 0 1 '(1.0) '(1.0)))

#;; Test: Normal-Normal model with circular condition
;; Preimage should look like a football set up for a field goal
(begin
  (interval-max-splits 0)
  
  (define/drbayes (hypot x y)
    (sqrt (+ (sqr x) (sqr y))))
  
  (define/drbayes e
    (let* ([x0  (normal 0 1)]
           [x1  (normal x0 1)])
      (list x0 x1 (hypot x0 x1))))
  
  (define B (set-list reals reals (real-set 0.99 1.01))))

#;; Test: thermometer that goes to 100
(begin
  (interval-max-splits 4)
  
  (define e
    (drbayes
     (let* ([x  (normal 90 10)]
            [y  (normal x 1)])
       (list x (if (y . > . 100) 100 y)))))
  
  (define B (set-list reals (real-set 99.0 100.0))))

(define ε 0.5)

(: interval-near (Flonum -> Set))
(define (interval-near x)
  (real-set (- x ε) (+ x ε)))

#;; Test: Normal-Normal model with more observations
;; Density plot, mean, and stddev should be similar to those produced by `normal-normal/lw'
(begin
  (interval-max-splits 2)
  ;(interval-min-length (flexpt 0.5 5.0))
  
  (define/drbayes e
    (let ([x  (normal 0 1)])
      (list x
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1))))
  (define B
    (set-list reals
              (interval-near 3.3)
              (interval-near 2.0)
              (interval-near 1.0)
              (interval-near 0.2)
              (interval-near 1.5)
              (interval-near 2.4)))
  
  (normal-normal/lw 0 1 '(3.3 2.0 1.0 0.2 1.5 2.4) '(1.0 1.0 1.0 1.0 1.0 1.0)))

;; ===================================================================================================

(define-values (f h idxs)
  (match-let ([(meaning _ f h k)  e])
    (values (run/bot* f '()) (run/pre* h '()) (k '()))))

(define (empty-set-error)
  (error 'drbayes-sample "cannot sample from the empty set"))

(define refine
  (if (empty-set? B) (empty-set-error) (preimage-refiner h B)))

(define S
  (let ([S  (refine (cons omegas traces))])
    (if (empty-set? S) (empty-set-error) S)))

(match-define (cons R T) S)

(printf "idxs = ~v~n" idxs)
(printf "R = ~v~n" R)
(printf "T = ~v~n" T)
(newline)

(struct: domain-sample ([S : Nonempty-Store-Rect]
                        [s : Store]
                        [b : Maybe-Value]
                        [measure : Flonum]
                        [prob : Flonum]
                        [point-prob : Flonum]
                        [weight : Flonum])
  #:transparent)

(: accept-sample? (domain-sample -> Boolean))
(define (accept-sample? s)
  (define b (domain-sample-b s))
  (and (not (bottom? b))
       (set-member? B b)))

(: orig-samples (Listof store-rect-sample))
(define orig-samples
  (time
   ;profile-expr
   (refinement-sample* S idxs refine n)))

(: all-samples (Listof domain-sample))
(define all-samples
  (time
   ;profile-expr
   (let: loop : (Listof domain-sample) ([orig-samples : (Listof store-rect-sample)  orig-samples])
     (cond
       [(empty? orig-samples)  empty]
       [else
        (define s (first orig-samples))
        (match-define (store-rect-sample S m p) s)
        (define pt (refinement-sample-point S idxs refine))
        ;(match-define (cons R T) S)
        ;(define r (omega-set-sample-point R))
        ;(define t (trace-set-sample-point T))
        ;(define pt (store-sample (cons r t) m))
        (match pt
          [(store-sample s q)
           (define b (f (cons s null)))
           (cons (domain-sample S s b m p q (/ q p)) (loop (rest orig-samples)))]
          [_
           (define r (omega-set-sample-point R))
           (define t (trace-set-sample-point T))
           (define s (cons r t))
           (define b (bottom (delay "refinement-sample-point failed")))
           (cons (domain-sample S s b m p m (/ m p)) (loop (rest orig-samples)))])]))))

(newline)

(define samples (filter accept-sample? all-samples))
(define ws (map domain-sample-weight samples))
(define ps (map domain-sample-prob samples))
(define ms (map domain-sample-measure samples))

(define not-samples (filter (compose not accept-sample?) all-samples))

(define num-all-samples (length all-samples))
(define num-samples (length samples))
(define num-not-samples (length not-samples))

(define accept-prob (fl (/ num-samples num-all-samples)))

(printf "search stats:~n")
(get-search-stats)
(newline)

#|
(printf "cache stats:~n")
(get-cache-stats)
(newline)
|#

(printf "unique numbers of primitive rvs: ~v~n"
        (sort
         (remove-duplicates
          (map (λ: ([d : domain-sample])
                 (length (omega-set->list (car (domain-sample-S d)))))
               all-samples))
         <))
(newline)

(printf "accepted samples: ~v (~v%)~n" (length samples) (* 100.0 accept-prob))
(newline)

(define all-alpha (min 1.0 (/ 250.0 (fl num-all-samples))))
(define alpha (min 1.0 (/ 250.0 (fl num-samples))))

(plot-z-ticks no-ticks)
(plot3d (list (rectangles3d (append*
                             (map (λ: ([d : domain-sample])
                                    (omega-rect->plot-rects (car (domain-sample-S d))))
                                  not-samples))
                            #:alpha all-alpha #:color 1 #:line-color 1)
              (rectangles3d (append*
                             (map (λ: ([d : domain-sample])
                                    (omega-rect->plot-rects (car (domain-sample-S d))))
                                  samples))
                            #:alpha all-alpha #:color 3 #:line-color 3))
        #:x-min 0 #:x-max 1 #:y-min 0 #:y-max 1 #:z-min 0 #:z-max 1)

(: domain-sample->omega-point (domain-sample -> (Listof Flonum)))
(define (domain-sample->omega-point d)
  (omega->point (car (domain-sample-s d))))

(plot3d (list (points3d (map domain-sample->omega-point not-samples)
                        #:sym 'dot #:size 12 #:alpha all-alpha #:color 1 #:fill-color 1)
              (points3d (map domain-sample->omega-point samples)
                        #:sym 'dot #:size 12 #:alpha all-alpha #:color 3 #:fill-color 3))
        #:x-min 0 #:x-max 1 #:y-min 0 #:y-max 1 #:z-min 0 #:z-max 1
        #:x-label "x1" #:y-label "x2" #:z-label "x3")

(plot3d (points3d (sample (discrete-dist (map domain-sample->omega-point samples) ws)
                          num-samples)
                  #:sym 'dot #:size 12 #:alpha alpha)
        #:x-min 0 #:x-max 1 #:y-min 0 #:y-max 1 #:z-min 0 #:z-max 1
        #:x-label "x1" #:y-label "x2" #:z-label "x3")

(: xss (Listof (Listof Flonum)))
(define xss
  (map (λ: ([d : domain-sample])
         (define lst (value->listof-flonum (cast (domain-sample-b d) Value)))
         (maybe-pad-list lst 3 random))
       samples))

(with-handlers ([exn?  (λ (_) (printf "image points scatter plot failed~n"))])
  (plot3d (points3d xss #:sym 'dot #:size 12 #:alpha alpha)
          #:x-label "x1" #:y-label "x2" #:z-label "x3"))

(with-handlers ([exn?  (λ (_) (printf "resampled image points scatter plot failed~n"))])
  (plot3d (points3d (sample (discrete-dist xss ws) num-samples)
                    #:sym 'dot #:size 12 #:alpha alpha)
          #:x-label "x1" #:y-label "x2" #:z-label "x3"))

(define x0s (map (inst first Flonum Flonum) xss))

(with-handlers ([exn?  (λ (_) (printf "weight density plot failed~n"))])
  (plot (density ws) #:x-label "weight" #:y-label "density"))

(with-handlers ([exn?  (λ (_) (printf "weight/measure scatter plot failed~n"))])
  (plot (points (map (λ: ([w : Flonum] [m : Flonum]) (list w m)) ws ms)
                #:sym 'dot #:size 12 #:alpha alpha)
        #:x-label "weight" #:y-label "measure"))

(printf "Corr(W,M) = ~v~n" (correlation ws ms))

(with-handlers ([exn?  (λ (_) (printf "density plot failed~n"))])
  (plot (density (sample (discrete-dist x0s ws) num-samples) 2)
        #:x-label "x0" #:y-label "density"))

(printf "E[x0] = ~v~n" (mean x0s (ann ws (Sequenceof Real))))
(printf "sd[x0] = ~v~n" (stddev x0s (ann ws (Sequenceof Real))))

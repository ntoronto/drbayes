#lang typed/racket

(require drbayes
         plot/typed
         plot/typed/utils
         (prefix-in
          math.
          (combine-in
           math/distributions
           math/statistics
           math/flonum
           math/base))
         "../test-utils.rkt"
         "normal-normal.rkt")

(printf "starting...~n~n")

(drbayes-sample-max-splits 0)

(: rect-alpha-adjust (Parameterof Nonnegative-Flonum))
(define rect-alpha-adjust (make-parameter 1.0))

(: req-samples (Parameterof Natural))
(define req-samples (make-parameter 1000))

(: 3d-preimage-plot? (Parameterof Boolean))
(define 3d-preimage-plot? (make-parameter #t))

(: 3d-image-plot? (Parameterof Boolean))
(define 3d-image-plot? (make-parameter #t))

(define/drbayes (normal/box-muller)
  (* (sqrt (* -2 (log (random))))
     (partial-cos (* (const pi) (uniform -1 1)))))

;#|
;; Test: constants
;; Preimage is unrestricted
(drbayes-sample-max-splits 3)
(3d-preimage-plot? #t)
(3d-image-plot? #f)
(define/drbayes (e) #f)
(define/drbayes (g e) (not e))
;|#

#|
;; Test: random
;; Preimage is [0.25,0.5]
(drbayes-sample-max-splits 4)
(3d-preimage-plot? #f)
(3d-image-plot? #f)
(define/drbayes (e) (random))
(define/drbayes (g x) (<= 0.25 x 0.5))
|#

#|
;; Test: cons
;; Preimage is [0.25,0.5] x [0.25,0.5]
(drbayes-sample-max-splits 4)
(req-samples 200)
(3d-preimage-plot? #f)
(3d-image-plot? #f)
(define/drbayes (e) (cons (random) (random)))
(define/drbayes (g x)
  (and (<= 0.25 (car x) 0.5)
       (<= 0.25 (cdr x) 0.5)))
|#

#|
;; Test: lists
;; Preimage is [0.25,0.5] x [0.25,0.5] x [0.25,0.5]
(drbayes-sample-max-splits 2)
(req-samples 1000)
(define/drbayes (e) (list (random) (random) (random)))
(define/drbayes (g0 x) (<= 0.25 x 0.75))
(define/drbayes (g x) (and (g0 (list-ref x 0))
                           (g0 (list-ref x 1))
                           (g0 (list-ref x 2))))
|#

#|
;; Not well-defined (won't expand)
(define/drbayes (e) (e))
(define/drbayes (g e) #t)
|#

#|
;; Preimage is ∅
(req-samples 200)
(drbayes-always-terminate? #t)
(define/drbayes (e) (if #t (e) (e)))
(define/drbayes (g e) #t)
|#

#|
;; Always terminates, but doesn't look like it abstractly
;; Preimage is Ω
(drbayes-sample-max-splits 5)
(drbayes-always-terminate? #t)
(3d-preimage-plot? #f)
(3d-image-plot? #f)

(define/drbayes (loop)
  (if #t (loop) (loop)))

(define/drbayes (e)
  (let ([x  (random)]
        [y  (random)])
    (if (x . < . y)
        (if (x . >= . y) (loop) 0)
        (if (x . < . y) (loop) 0))))

(define/drbayes (g e) #t)
|#

#|
;; Terminates with probability 0.5
;; Preimage is [0,0.5] × [0.25,0.75]
(drbayes-sample-max-splits 5)
(drbayes-always-terminate? #t)
(3d-preimage-plot? #f)
(3d-image-plot? #f)

(define/drbayes (loop) (if #t (loop) (loop)))

(define/drbayes (e)
  (if (boolean 0.5) (random) (loop)))

(define/drbayes (g x)
  (and (< 0.25 x 0.75)))
|#

#|
;; Terminates with probability zero
(req-samples 200)
(drbayes-sample-max-splits 0)
(drbayes-always-terminate? #t)
(3d-preimage-plot? #f)
(3d-image-plot? #f)

(define/drbayes (loop) (if #t (loop) (loop)))

(define/drbayes (e)
  (if (<= (random) 0) (random) (loop)))

(define/drbayes (g x) #t)
|#

#|
;; Geometric distribution (terminates with probability 1)
(drbayes-sample-max-splits 0)
(drbayes-always-terminate? #t)
(3d-image-plot? #f)

(define/drbayes (geometric p)
  (if (< (random) p) 0 (+ 1 (geometric p))))

(define/drbayes (e) (geometric 0.5))

(define/drbayes (g x) #t)
|#

#|
;; Always terminates with 0 or 1, but abstractly may not terminate
(drbayes-sample-max-splits 2)  ; more splits yields more samples
(3d-preimage-plot? #f)

(define/drbayes (badfun x y)
  (if (x . < . y)
      (if (x . >= . y) (badfun x y) 0)
      (if (x . < . y) (badfun x y) 1)))

(define/drbayes (e)
  (badfun (random) (random)))

(define/drbayes (g x) #t)
|#

#|
;; Test: sqr
;; Preimage is [0.5,sqrt(1/2)]
(3d-preimage-plot? #f)
(define/drbayes (e) (sqr (random)))
(define/drbayes (g x) (<= 0.25 x 0.5))
|#

#|
;; Test: list, sqr
;; Preimage is a rectangle [0.25,0.5] x [0.5,sqrt(1/2)] x [0.5,sqrt(1/2)]
(define/drbayes (e) (list (random) (sqr (random)) (sqr (random))))
(define/drbayes (g0 x) (and (<= 0.25 x 0.5)))
(define/drbayes (g x) (and (g0 (list-ref x 0))
                           (g0 (list-ref x 1))
                           (g0 (list-ref x 2))))
|#

#|
;; Test: random, let
;; Preimage is [0.25,0.5]
(3d-preimage-plot? #f)
(define/drbayes (e) (let ([x (random)]) x))
(define/drbayes (g x) (<= 0.25 x 0.5))
|#

#|
;; Test: sqr, let
;; Preimage is [0.5,sqrt(1/2)]
(3d-preimage-plot? #f)
(define/drbayes (e) (let ([x  (sqr (random))]) x))
(define/drbayes (g x) (<= 0.25 x 0.5))
|#

#|
;; Test: list, sqr, let
;; Preimage is [0.25,0.5] × [0.5,sqrt(1/2)] x [0.5,sqrt(1/2)]
(define/drbayes (e)
  (let ([lst  (list (random) (sqr (random)) (sqr (random)))])
    (list (list-ref lst 1)
          (list-ref lst 0)
          (list-ref lst 2))))
(define/drbayes (g0 x) (<= 0.25 x 0.5))
(define/drbayes (g x) (and (g0 (list-ref x 0))
                           (g0 (list-ref x 1))
                           (g0 (list-ref x 2))))
|#

#|
;; Test: reciprocal
;; Preimage is [0.25,0.5] x [0.25,0.5] x [0.25,0.5]
(define/drbayes (e) (list (random) (/ (random)) (/ (random))))
(define/drbayes (g x)
  (and (<= 0.25 (list-ref x 0) 0.5)
       (<= 2.0 (list-ref x 1) 4.0)
       (<= 2.0 (list-ref x 2) 4.0)))
|#

#|
;; Test: addition
;; Preimage is a 2D downward diagonal strip
(req-samples 10000)
(3d-preimage-plot? #f)
(drbayes-sample-max-splits 5)
(define/drbayes (e) (+ (random) (random)))
(define/drbayes (g x) (<= 0.3 x 0.7))
|#

#|
;; Test: same as above, with let
(req-samples 10000)
(drbayes-sample-max-splits 5)
(3d-preimage-plot? #f)

(define/drbayes (e)
  (let ([x  (random)]
        [y  (random)])
    (list x y (+ x y))))

(define/drbayes (g xs)
  (<= 0.3 (list-ref xs 2) 0.7))
|#

#|
;; Test: sign of normal-distributed random variable
;; Preimage should be:
;;    #t: [0,0.5)
;;    #f: [0.5,1]
;;  both: [0,1]
(3d-preimage-plot? #f)
(define/drbayes (e) (negative? (normal-inv-cdf (store-uniform))))
(define/drbayes (g x) x)
|#

#|
;; Test: less than
;; Preimage should be:
;;    #t: upper triangle
;;    #f: lower triangle and diagonal (though the diagonal isn't detectable)
;;  both: [0,1] × [0,1]
(3d-preimage-plot? #f)
(drbayes-sample-max-splits 5)
(define/drbayes (e) (< (random) (random)))
;(define/drbayes (g x) (equal? x #t))
(define/drbayes (g x) (equal? x #f))
;(define/drbayes (g x) #t)
|#

#|
;; Test: random boolean
;; Preimage should be [0,0.4)
(3d-preimage-plot? #f)
(drbayes-sample-max-splits 0)
(define/drbayes (e) (boolean 0.4))
(define/drbayes (g x) x)
|#

#|
;; Test: simplest if
;; Preimage should be unrestricted
(drbayes-sample-max-splits 0)
(define/drbayes (e) (if #t #t #f))
(define/drbayes (g x) x)
|#

#|
;; Test: simple if
;; Preimage should be [0,0.4]
(drbayes-sample-max-splits 0)
(define/drbayes (e) (if (boolean 0.4) #t #f))
(define/drbayes (g x) x)
|#

#|
;; Test: if
;; Preimage should be the union of a large upper triangle and a small lower triangle, and
;; samples should be uniformly distributed
(drbayes-sample-max-splits 3)
(3d-preimage-plot? #f)
(3d-image-plot? #f)

(define/drbayes (e)
  (let ([x  (random)]
        [y  (random)])
    (list x y (strict-if (< x y) #t (> x (scale y (const 8)))))
    ;(list x y (if (< x y) #t (> x (scale y (const 8)))))
    ))

(define/drbayes (g xs)
  (list-ref xs 2))
|#

#|
;; Test: Normal-Normals with different interval widths
(drbayes-sample-max-splits 2)
(req-samples 1000)
(rect-alpha-adjust 0.05)

(define/drbayes (e)
  (let ([x  (normal 0 1)])
    (list x (normal x 1) (normal x 1))))

(define/drbayes (g xs)
  (and (<= 1.99 (list-ref xs 1) 2.01)
       (<= -1.2 (list-ref xs 2) -0.8)))

(normal-normal/lw 0 1 '(2.0 -1.0) '(1.0 1.0))
|#

#|
;; Test: Normal-Normals with different noise instead of different widths
(drbayes-sample-max-splits 3)

(define/drbayes (e)
  (let* ([x  (normal 0 1)]
         [x1  (normal x 1)]
         [x2  (normal x 1)])
    (list x (normal x1 0.01) (normal x2 0.2))))

(define/drbayes (g xs)
  (and (<= 1.9999 (list-ref xs 1) 2.0001)
       (<= -1.0001 (list-ref xs 2) -0.9999)))

(normal-normal/lw 0 1 '(2.0 -1.0) '(1.0 1.0))
|#

#|
;; Test: observing sum of normals
(drbayes-sample-max-splits 0)
(3d-image-plot? #f)
(req-samples 10000)

(define/drbayes (e)
  (let* ([x  (normal 0 1)]
         [y1  (normal x 1)]
         [y2  (normal x 1)])
    (list x (+ y1 y2))))

(define/drbayes (g xs)
  (<= 1.9 (list-ref xs 1) 2.1))

(normal-normal/lw 0 1 '(2.0) (list (flsqrt 2.0)))
|#

#|
;; Quadratic fit
(begin
  (drbayes-sample-max-splits 0)
  
  (define xs (list 0 1 2 3 4 5 6 7
                   8 9 10 11 12 13 14 15
                   ))
  (define ys* (list 0.063 0.262 0.493 0.814 1.222 1.708 2.238 2.883
                    3.680 4.393 5.382 6.377 7.517 8.547 9.592 11.044
                    ))
  
  (define/drbayes (quadratic-eval a0 a1 a2 x)
    (+ a0 (* x (+ a1 (* x a2)))))
  
  (define/drbayes (generate a0 a1 a2 xs)
    (if (null? xs)
        null
        (cons (let ([x  (car xs)])
                (normal (quadratic-eval a0 a1 a2 x) 1))
              (generate a0 a1 a2 (cdr xs)))))
  
  (define/drbayes (condition ys ys*)
    (if (null? ys)
        #t
        (and (let ([y   (car ys)]
                   [y*  (car ys*)])
               (and (<= (- y* 0.001) y)
                    (<= y (+ y* 0.001))))
             (condition (cdr ys) (cdr ys*)))))
  
  (define/drbayes (e)
    (let* ([a0  (cauchy 0 0.1)]
           [a1  (cauchy 0 0.1)]
           [a2  (cauchy 0 0.1)]
           [ys  (generate a0 a1 a2 (const xs))])
      (list a0 a1 a2 (condition ys (const ys*)))))
  
  (define/drbayes (g xs)
    (list-ref xs 3))
  )
|#

#|
;; Dependency problem
(begin
  (drbayes-sample-max-splits 1)
  
  (define/drbayes (e)
    (let ([x  (random)]
          [y  (random)])
      ;(/ x (+ x y))  ; has dependency problem
      (/ 1 (+ 1 (/ y x)))  ; doesn't
      ))
  
  (define/drbayes (g x)
    (and (<= 0.4 x) (<= x 0.6))))
|#

#|
;; Model selection
(begin
  (drbayes-sample-max-splits 0)
  
  ;; Can't get exponential samples if we include all the data
  (define xs (list 0 1 2 3 4 5 6 7
                   ;8 9 10 11 12 13 14 15
                   ))
  (define ys* (list 0.063 0.262 0.493 0.814 1.222 1.708 2.238 2.883
                    ;3.680 4.393 5.382 6.377 7.517 8.547 9.592 11.044
                    ))
  
  (define/drbayes (exponential-eval a0 a1 x)
    (+ a0 (* a1 (exp (* x (const (log 2)))))))
  
  (define/drbayes (generate-exponential a0 a1 xs)
    (if (null? xs)
        null
        (cons (let ([x  (car xs)])
                (normal (exponential-eval a0 a1 x) 1))
              (generate-exponential a0 a1 (cdr xs)))))
  
  (define/drbayes (quadratic-eval a0 a1 a2 x)
    (+ a0 (* x (+ a1 (* x a2)))))
  
  (define/drbayes (generate-quadratic a0 a1 a2 xs)
    (if (null? xs)
        null
        (cons (let ([x  (car xs)])
                (normal (quadratic-eval a0 a1 a2 x) 1))
              (generate-quadratic a0 a1 a2 (cdr xs)))))
  
  (define/drbayes (condition ys ys*)
    (if (null? ys)
        #t
        (and (let ([y   (car ys)]
                   [y*  (car ys*)])
               (and (<= (- y* 0.001) y)
                    (<= y (+ y* 0.001))))
             (condition (cdr ys) (cdr ys*)))))
  
  (define/drbayes (e)
    (if (boolean 0.5)
        (let* ([a0  (cauchy 0 0.1)]
               [a1  (cauchy 0 0.1)]
               [a2  (cauchy 0 0.1)]
               [ys  (generate-quadratic a0 a1 a2 (const xs))])
          (list #t (list a0 a1 a2) (condition ys (const ys*))))
        (let* ([a0  (cauchy 0 0.1)]
               [a1  (* -0.5 (log (random)))]
               [ys  (generate-exponential a0 a1 (const xs))])
          (list #f (list a0 a1) (condition ys (const ys*))))))
  
  (define/drbayes (g xs)
    (list-ref xs 2))
  )
|#

#|
;; Test: Box-Muller method for generating standard normal samples
(begin
  (drbayes-sample-max-splits 3)
  
  (define/drbayes (e)
    (let* ([x  (normal/box-muller)]
           [y  (+ x (normal/box-muller))])
      (list x y)))
  
  (define/drbayes (g xs)
    (and (<= 1.8 (list-ref xs 1)) (<= (list-ref xs 1) 2.2)))
  
  (rect-alpha-scale 1/2)
  )
|#

#|
;; Test: multiplication
(begin
  (drbayes-sample-max-splits 4)
  
  (define/drbayes (e)
    (let* ([x  (random-std-cauchy)]
           [y  (random-std-cauchy)])
      (list x y (* x y))))
  
  (define/drbayes (g xs)
    (and (<= -0.1 (list-ref xs 2))
         (<= (list-ref xs 2) 0.2))))
|#

#|
;; Test: sqr
;; Preimage should look like the graph of the function
(begin
  (drbayes-sample-max-splits 1)
  
  (define/drbayes (e)
    (let ([x  (uniform -1 1)]
          [y  (uniform 0 1)])
      (list x y (- y (sqr x)))))
  
  (define/drbayes (g xs)
    (and (<= -0.1 (list-ref xs 2))
         (<= (list-ref xs 2) 0.1))))
|#

#|
;; Test: sine and cosine restricted to [-π,π]
;; Looked at top-down, the plots should look like the graphs of the functions
(begin
  (drbayes-sample-max-splits 1)
  
  (define/drbayes (e)
    (let ([x  (uniform (const (- pi)) (const pi))]
          [y  (uniform -1.1 1.1)])
      (list x y
            ;(- y (partial-cos x))
            (- y (partial-sin x))
            )))
  
  (define/drbayes (g xs)
    (and (<= -0.1 (list-ref xs 2))
         (<= (list-ref xs 2) 0.1))))
|#

#|
;; Test: asin and acos
;; Looked at top-down, the plots should look like the graphs of the functions
(begin
  (drbayes-sample-max-splits 1)
  #;
  (define/drbayes (e)
    (let ([x  (uniform -1 1)]
          [y  (uniform 0 (const pi))])
      (list x y (- y (acos x)))))
  
  (define/drbayes (e)
    (let ([x  (uniform -1 1)]
          [y  (uniform (const (* -0.5 pi)) (const (* 0.5 pi)))])
      (list x y (- y (asin x)))))
  
  (define/drbayes (g xs)
    (and (<= -0.1 (list-ref xs 2))
         (<= (list-ref xs 2) 0.1))))
|#

#|
;; Test: Normal-Normal model
;; Preimage should be a banana shape
(drbayes-sample-max-splits 5)
(3d-preimage-plot? #f)
(3d-image-plot? #f)
(req-samples 1000)

(define/drbayes (e)
  (let* ([x  (normal 0 1)]
         [y  (normal x 1)])
    (list x (<= 1.9 y 2.1))))

(define/drbayes (g xs)
  (list-ref xs 1))

(normal-normal/lw 0 1 '(2.0) '(1.0))
|#

#|
;; Test: Normal-Normal with first variable floored
(begin
  (drbayes-sample-max-splits 2)
  
  (define/drbayes (e)
    (let* ([x  (normal 0 8)]
           [y  (normal (floor x) 1)])
      (list x y)))
  
  (define/drbayes (g xs)
    (and (<= -0.1 (list-ref xs 1))
         (<= (list-ref xs 1) 0.1))))
|#

#|
;; Test: thermometer that goes to 100
(begin
  (drbayes-sample-max-splits 3)
  ;(interval-min-length 0.0)
  
  (define/drbayes (e)
    (let* ([x  (normal 90 10)]
           [y  (+ x (normal 0 1))])
      (list x
            ;(strict-if (y . > . 100) 100 (strict-if (y . < . 0) 0 y))
            (if (y . > . 100) 100 (if (y . < . 0) 0 y))
            )))
  
  (define/drbayes (g xs)
    (= (list-ref xs 1) 100)))
|#

#|
;; Test: Normal-Normal model with circular condition
;; Preimage should look like a football set up for a field goal
(begin
  (drbayes-sample-max-splits 3)
  
  (define/drbayes (mysqr x)
    (if (negative? x) (sqr x) (sqr x)))
  
  (define/drbayes (hypot x y)
    (sqrt (+ (sqr x) (sqr y))))
  
  (define/drbayes (e)
    (let* ([x0  (normal 0 1)]
           [x1  (normal x0 1)])
      (list x0 x1 (hypot x0 x1))))
  
  (define/drbayes (g xs)
    (and (<= 0.95 (list-ref xs 2))
         (<= (list-ref xs 2) 1.05))))
|#

#|
;; Test: random square, with obviously repeated variable in condition
(begin
  (drbayes-sample-max-splits 2)
  
  (define/drbayes e
    (let* ([x0  (random)]
           [x1  (random)])
      (list x0 x1 (sqrt (+ (sqr x0) (sqr (+ x0 x1)))))))
  
  (define B (set-list reals reals (real-set 0.99 1.01))))
|#

#|
;; Test: Normal-Normal or Cauchy-Cauchy, depending on random variable
(begin
  (drbayes-sample-max-splits 4)
  (define/drbayes e
    (if
     ;((random) . < . (const #i499/1000))
     (boolean #i499/1000)
     (let ([x  (random-std-normal)])
       (list x (normal x 1)))
     (let ([x  (random-std-cauchy)])
       (list x (cauchy x 1)))))
  (define B (set-list reals (real-set 0.9 1.1))))
|#

#|
;; Test: Boolean(p) distribution
;; Preimage should be [0,p); sampler should fail only once
(begin
  (define p #i2/5)
  (define e (drbayes (if (boolean (const p)) #t #f)))
  (define B trues))
|#

#|
;; Test: List length distributed Geometric(0.5)
(begin
  (drbayes-sample-max-splits 2)
  
  (define/drbayes (geom)
    (if (boolean 0.5) (cons #t (geom)) null))
  
  (define e (drbayes (geom)))
  (define B universe))
|#

#|
;; Test: constrained Geometric(p) distribution
(begin
  (drbayes-sample-max-splits 1)
  (define p #i1/16)
  
  (define/drbayes (geometric-p)
    ;(if ((random) . < . (const p)) 0 (+ 1 (geometric-p)))
    (let ([x  (if (boolean (const p)) 0 (+ 1 (geometric-p)))
              ;(if ((random) . < . (const p)) 0 (+ 1 (geometric-p)))
              ])
      ;; Allows preimage sampling to prove (+ 1 (geometric-p)) > B-max if (geometric-p) >= B-max;
      ;; i.e. to discover that taking the "false" branch leads to failure after a certain depth
      ;x
      (strict-if (negative? x) (fail) x)
      ))
  
  (define/drbayes e (geometric-p))
  
  (define B-min 1.0)
  (define B-max 3.0)
  (define B (real-set B-min B-max #t #t))
  
  (let ([xs  (sample (truncated-dist (geometric-dist p) (- B-min 1.0) B-max) 50000)])
    (printf "E[x] = ~v~n" (mean xs))
    (printf "sd[x] = ~v~n" (stddev xs))))
|#

#|
;; Test: Conditioning on Geometric(0.5) distribution defined via recursion
;; Image points should lie on integer x coordinates and be clustered around 3
(begin
  (drbayes-sample-max-splits 1)
  
  (define p #i499/1000)
  
  (define/drbayes (geometric-p)
    ;(if ((random) . < . (const p)) 0 (+ 1 (geometric-p)))
    (let ([x  (if (boolean (const p)) 0 (+ 1 (geometric-p)))
              ;(if ((random) . < . (const p)) 0 (+ 1 (geometric-p)))
              ])
      (strict-if (negative? x) (fail) x))
    )
  
  (define/drbayes e
    (let ([x  (geometric-p)])
      (list x (normal x 1))))
  
  (define B (set-list reals (real-set 2.9 3.1 #t #t)))
  
  (let ()
    (define xs (sample (geometric-dist p) 10000))
    (define ws (map (λ: ([x : Flonum]) (pdf (normal-dist x) 3.0)) xs))
    (print
     (plot (density xs 1 ws) #:x-label "x" #:y-label "density"))
    (newline)
    (printf "E[x] = ~v~n" (mean xs (ann ws (Sequenceof Real))))
    (printf "sd[x] = ~v~n" (stddev xs (ann ws (Sequenceof Real))))))
|#

#|
;; Test: Normal-Normal model with more observations
;; Density plot, mean, and stddev should be similar to those produced by `normal-normal/lw'
(begin
  (drbayes-sample-max-splits 0)
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
              (real-set 3.2 3.4 #t #t)
              (real-set 1.9 2.1 #t #t)
              (real-set 0.9 1.1 #t #t)
              (real-set 0.1 0.3 #t #t)
              (real-set 1.4 1.6 #t #t)
              (real-set 2.3 2.5 #t #t)))
  (normal-normal/lw 0 1 '(3.3 2.0 1.0 0.2 1.5 2.4) '(1.0 1.0 1.0 1.0 1.0 1.0)))
|#

#|
;; Test: failure on a branch of lazy if
;; Preimage should be a lower right triangle; shouldn't have many failures
(begin
  (drbayes-sample-max-splits 2)
  (define/drbayes e
    (let ([x  (random)]
          [y  (random)]
          [z  (random)])
      (if (x . < . y) (fail) z)))
  (define B universe))
|#

#|
;; Test: tagging
;; Preimage should be [0,1]
(begin
  (define t0 (make-set-tag 't0))
  (define e (drbayes (tag (random) t0)))
  (define B (set-tag reals t0)))
|#

#|
;; Test: tagging and untagging
;; Preimage should be [0,1]
(begin
  (define t0 (make-set-tag 't0))
  (define e (drbayes (untag (tag (random) t0) t0)))
  (define B reals))
|#

#|
;; Test: Normal-Normal with circular condition and first variable tagged
(begin
  (define t0 (make-set-tag 't0))
  (define/drbayes e
    (let* ([x0  (tag (random-std-normal) t0)]
           [x1  (normal (untag x0 t0) 1)])
      (list (untag x0 t0) x1 (sqrt (+ (sqr (untag x0 t0)) (sqr x1))))))
  (define B (set-list reals reals (real-set 0.95 1.05))))
|#

#|
;; Test: Normal-Normal with circular condition and first variable tagged with probability 0.5
(begin
  (drbayes-sample-max-splits 0)
  (define t0 (make-set-tag 't0))
  (define/drbayes e
    (let* ([x0  (if ((random) . < . 0.45)
                    (tag (normal 0 1) t0)
                    (random-std-normal))]
           [y0  (if (tag? x0 t0) (untag x0 t0) x0)]
           [x1  (normal y0 1)])
      (list y0 x1 (sqrt (+ (sqr y0) (sqr x1))))))
  (define B (set-list reals reals (real-set 0.95 1.05))))
|#

;; ===================================================================================================

(define/drbayes (e*)
  (let ([x (e)])
    (condition x (g x))))

(define-values (Ss ss bs ws)
  (parameterize ([drbayes-sample-store-sets  empty]
                 [drbayes-sample-stores      empty])
    (define-values (bs ws) (drbayes-sample (drbayes (e*)) (req-samples)))
    (values (drbayes-sample-store-sets)
            (drbayes-sample-stores)
            bs
            ws)))

(define xss (map value->point bs))

(define num-samples (length bs))
(define accept-prob (/ num-samples (req-samples)))

(printf "search stats:~n")
(get-search-stats)
(newline)

#|
(printf "cache stats:~n")
(get-cache-stats)
(newline)
|#

(printf "unique numbers of primitive rvs: ~v~n"
        (sort (remove-duplicates
               (map (λ: ([S : Nonempty-Store-Set]) (length (store-set-random-list S))) Ss))
              <))
(newline)

(printf "accepted samples: ~v (~v%)~n" num-samples (* 100.0 accept-prob))
(newline)

(define rect-alpha (* 0.75 (rect-alpha-adjust)))
(define value-alpha (alpha-clamp (/ 500.0 (math.fl num-samples))))

(with-handlers ([exn?  (λ (_) (printf "***** preimage plot failed *****~n~n"))])
  (cond
    [(3d-preimage-plot?)
     (plot3d (list (store-sets-renderer3d Ss rect-alpha)
                   (values-renderer3d ss ws 12.0 value-alpha))
             #:x-min 0 #:x-max 1 #:y-min 0 #:y-max 1 #:z-min 0 #:z-max 1
             #:x-label "ω0" #:y-label "ω1" #:z-label "ω2")]
    [else
     (displayln
      (plot (list (store-sets-renderer2d Ss rect-alpha)
                  (values-renderer2d ss ws 12.0 value-alpha))
            #:x-min 0 #:x-max 1 #:y-min 0 #:y-max 1
            #:x-label "ω0" #:y-label "ω1"))]))

(with-handlers ([exn?  (λ (_) (printf "***** image plot failed *****~n~n"))])
  (cond
    [(3d-image-plot?)
     (plot3d (values-renderer3d bs ws 12.0 value-alpha)
             #:x-label "x0" #:y-label "x1" #:z-label "x2")]
    [else
     (plot (values-renderer2d bs ws 12.0 value-alpha)
           #:x-label "x0" #:y-label "x1")]))

(define x0s (map (inst first Flonum Flonum) xss))

(with-handlers ([exn?  (λ (_) (printf "***** weight density plot failed *****~n~n"))])
  (plot (density ws) #:x-label "weight" #:y-label "density"))

(with-handlers ([exn?  (λ (_) (printf "***** density plot failed *****~n~n"))])
  (plot (list (density x0s 1 ws #:width 2 #:label "Est. Density"))
        #:x-label "x"
        #:y-label "density"))

(printf "E[x0] = ~v~n" (math.mean x0s ws))
(printf "sd[x0] = ~v~n" (math.stddev x0s ws))

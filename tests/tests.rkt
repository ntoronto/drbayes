#lang typed/racket

(require plot/typed
         math/distributions
         math/statistics
         math/flonum
         "../main.rkt"
         "test-utils.rkt"
         "profile.rkt"
         "normal-normal.rkt")

(plot-font-size 12)

(printf "starting...~n~n")

(error-print-width 1024)

(: rect-alpha-scale (Parameterof Real))
(define rect-alpha-scale (make-parameter 1))

(: point-alpha-scale (Parameterof Real))
(define point-alpha-scale (make-parameter 2))

(interval-max-splits 0)
(define n 10000)

(define/drbayes (normal/box-muller)
  (* (sqrt (* -2 (log (random))))
     (partial-cos (* (const pi) (uniform -1 1)))))

;(define/drbayes e 3)

#;; Test: constants
;; Preimage is unrestricted
(begin
  (define e (drbayes #f))
  (define B falses))

#;; Not well-defined (won't expand)
(begin
  (define/drbayes (bad-loop) (bad-loop))
  (define/drbayes e (bad-loop))
  (define B universe))

#;; Preimage is ∅
(begin
  (drbayes-always-terminate? #t) 
  (define/drbayes (loop) (if #t (loop) (loop)))
  (define/drbayes e (loop))
  (define B universe))

#;; Always terminates, but doesn't look like it abstractly
;; Preimage is Ω
(begin
  (interval-max-splits 5)
  (drbayes-always-terminate? #t)
  
  (define/drbayes (loop)
    (if #t (loop) (loop)))
  
  (define/drbayes e
    (let ([x  (random)]
          [y  (random)])
      (if (x . < . y)
          (if (x . >= . y) (loop) 0)
          (if (x . < . y) (loop) 0))))
  
  (define B universe))

#;; Terminates with probability 0.5
;; Preimage is [0,0.5] × [0.25,0.75]
(begin
  (interval-max-splits 5)
  (drbayes-always-terminate? #t)
  
  (define/drbayes (loop) (if #t (loop) (loop)))
  
  (define/drbayes e
    (if (boolean (const 0.5)) (random) (loop)))
  
  (define B (real-set 0.25 0.75)))

#;; Terminates with probability zero
(begin
  (interval-max-splits 0)
  (drbayes-always-terminate? #t)
  
  (define/drbayes (loop) (if #t (loop) (loop)))
  
  (define/drbayes e
    (if (<= (random) 0) (random) (loop)))
  
  (define B universe))

#;
(begin
  (interval-max-splits 0)
  (drbayes-always-terminate? #t)
  
  (define/drbayes (geometric p)
    (if (< (random) p) 0 (+ 1 (geometric p))))
  
  (define/drbayes e (geometric 0.5))
  
  (define B universe))

#;
(begin
  (define/drbayes (badfun x y)
    (if (x . < . y)
        (if (x . >= . y) (badfun x y) 0)
        (if (x . < . y) (badfun x y) 1)))
  
  (define/drbayes e
    (badfun (random) (random)))
  
  (define B universe))

#;; Test: random
;; Preimage is [0.25,0.5]
(begin
  (define e (drbayes (random)))
  (define B (real-set 0.25 0.5 #t #t)))

#;; Test: cons
;; Preimage is [0.25,0.5] x [0.25,0.5]
(begin
  (interval-max-splits 1)
  (define e (drbayes (cons (random) (random))))
  (define I (real-set 0.25 0.5 #t #t))
  (define B (set-pair I I)))

#;; Test: improper cons-list
;; Preimage is [0.25,0.5] x [0.25,0.5] x [0.25,0.5]
(begin
  (interval-max-splits 1)
  (define e (drbayes (cons (random) (cons (random) (random)))))
  (define I (real-set 0.25 0.5 #t #t))
  (define B (set-pair I (set-pair I I))))

#;; Test: lists
;; Preimage is [0.25,0.5] x [0.25,0.5] x [0.25,0.5]
(begin
  (define e (drbayes (list (random) (random) (random))))
  (define I (real-set 0.25 0.5 #t #t))
  (define B (set-list I I I)))

#;; Test: sqr
;; Preimage is [0.5,sqrt(1/2)]
(begin
  (define e (drbayes (sqr (random))))
  (define B (real-set 0.25 0.5 #t #t)))

#;; Test: list, sqr
;; Preimage is a rectangle [0.25,0.5] x [0.5,sqrt(1/2)] x [0.5,sqrt(1/2)]
(begin
  (define e (drbayes (list (random) (sqr (random)) (sqr (random)))))
  (define I (real-set 0.25 0.5))
  (define B (set-list I I I)))

#;; Test: random, let
;; Preimage is [0.25,0.5]
(begin
  (define e (drbayes (let ([x  (random)]) x)))
  (define B (real-set 0.25 0.5 #t #t)))

#;; Test: sqr, let
;; Preimage is [0.5,sqrt(1/2)]
(begin
  (define e (drbayes (let ([x  (sqr (random))]) x)))
  (define B (real-set 0.25 0.5 #t #t)))

#;; Test: list, sqr, let
;; Preimage is [0.5,sqrt(1/2)] x [0.25,0.5] x [0.5,sqrt(1/2)]
(begin
  (define/drbayes e
    (let ([lst  (list (random) (sqr (random)) (sqr (random)))])
      (list (list-ref lst (const 1))
            (list-ref lst (const 0))
            (list-ref lst (const 2)))))
  (define I (real-set 0.25 0.5 #t #t))
  (define B (set-list I I I)))

#;; Test: inversion
;; Preimage is [0.25,0.5] x [0.25,0.5] x [0.25,0.5]
(begin
  (define e (drbayes (list (random) (/ (random)) (/ (random)))))
  (define I (real-set 2.0 4.0))
  (define B (set-list (real-set 0.25 0.5) I I)))

#;; Test: addition
;; Preimage is a 2D downard diagonal strip
(begin
  (interval-max-splits 5)
  (define e (drbayes (+ (random) (random))))
  (define B (real-set 0.3 0.7)))

#;; Test: same as above, with expressions
(begin
  (interval-max-splits 3)
  
  (define/drbayes e
    (let ([x  (random)]
          [y  (random)])
      (list x y (+ x y))))
  
  (define B (set-list reals reals (real-set 0.3 0.7))))

#;; Test: sign of normal-distributed random variable
;; Preimage should be:
;;    #t: [0,0.5)
;;    #f: [0.5,1]
;;  both: [0,1]
(begin
  (define e (drbayes (negative? (random-std-normal))))
  (define B trues)
  ;(define B falses)
  ;(define B bools)
  )

#;; Test: less than
;; Preimage should be:
;;    #t: upper triangle
;;    #f: lower triangle and diagonal (though the diagonal isn't detectable)
;;  both: [0,1] × [0,1]
(begin
  (interval-max-splits 3)
  (define e (drbayes ((random) . < . (random))))
  (define B trues)
  ;(define B falses)
  ;(define B bools)
  )

#;; Test: random boolean
;; Preimage should be [0,0.4)
(begin
  (interval-max-splits 0)
  (define e (drbayes (boolean (const 0.4))))
  (define B trues))

#;; Test: simplest if
;; Preimage should be unrestricted
(begin
  (interval-max-splits 0)
  (define e (drbayes (if #t #t #f)))
  (define B trues))

#;; Test: simple if
;; Preimage should be [0,0.4]
(begin
  (interval-max-splits 0)
  (define e (drbayes (if (boolean (const 0.4)) #t #f)))
  (define B trues))

#;; Test: if
;; Preimage should be the union of a large upper triangle and a small lower triangle, and
;; samples should be uniformly distributed
(begin
  (interval-max-splits 3)
  
  (define/drbayes e
    (let ([x  (random)]
          [y  (random)])
      ;(list x y (strict-if (< x y) #t (> x (scale y (const 8)))))
      (list x y (if (< x y) #t (> x (scale y (const 8)))))
      ))
  (define B (set-list reals reals trues)))

#;; Test: Normal-Normals with different interval widths
(begin
  (interval-max-splits 2)
  
  (define/drbayes e
    (let ([x  (normal 0 1)])
      (list x (normal x 1) (normal x 1))))
  
  (define B (set-list reals (real-set 1.99 2.01) (real-set -1.2 -0.8)))
  
  (normal-normal/lw 0 1 '(2.0 1.0) '(1.0 1.0)))

#;; Test: Normal-Normals with different noise instead of different widths
(begin
  (interval-max-splits 3)
  
  (define/drbayes e
    (let ([x  (normal 0 1)])
      (list x
            (normal (normal x 1) 0.2)
            (normal (normal x 1) 0.01))))
  
  (define B (set-list reals (real-set 1.9999 2.0001) (real-set -1.0001 -0.9999)))
  
  (normal-normal/lw 0 1 '(2.0 1.0) '(1.0 1.0)))

;; Test: observing sum of normals
(begin
  (interval-max-splits 0)
  
  (define/drbayes e
    (let* ([x  (normal 0 1)]
           [y1  (normal x 1)]
           [y2  (normal x 1)])
      (list x (+ y1 y2))))
  
  (define B (set-list reals (real-set 1.9 2.1)))
  
  (normal-normal/lw 0 2 '(2.0) (list (flsqrt 2.0))))

#;; Test: Normal-Normals with different numbers of observations
(begin
  (interval-max-splits 0)
  
  (define num 50)
  
  (define/drbayes e
    (let ([x  (normal 0 1)])
      (list x
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            (normal x 1)
            )))
  
  (: observation (Flonum -> Set))
  (define (observation x)
    (real-set (- x 0.2) (+ x 0.2)))

  (define B
    (apply set-list reals
           (build-list num (λ ([i : Index])
                             (observation (fl (+ 1/2 (* 1/4 (- i (/ num 2)))))))))))

#;; Quadratic fit
(begin
  (interval-max-splits 0)
  
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
  
  (define/drbayes e
    (let* ([a0  (cauchy 0 0.1)]
           [a1  (cauchy 0 0.1)]
           [a2  (cauchy 0 0.1)]
           [ys  (generate a0 a1 a2 (const xs))])
      (list a0 a1 a2 (condition ys (const ys*)))))
  
  (define B (set-list reals reals reals trues))
  )

#;; Dependency problem
(begin
  (interval-max-splits 1)
  
  (define/drbayes e
    (let ([x  (random)]
          [y  (random)])
      (/ 1 (+ 1 (/ y x)))
      ;(/ x (+ x y))
      #;
      (let ([z  (/ x (+ x y))])
        (strict-if (and (z . >= . 0) (z . <= . 1)) z (fail)))))
  
  (define B (real-set 0.4 0.6)))

;{ <x,z> in [0,1] × [0,2] : z - y = x }

#;; Model selection
(begin
  (interval-max-splits 0)
  
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
  
  (define/drbayes e
    (if (boolean (const 0.5))
        (let* ([a0  (cauchy 0 0.1)]
               [a1  (cauchy 0 0.1)]
               [a2  (cauchy 0 0.1)]
               [ys  (generate-quadratic a0 a1 a2 (const xs))])
          (list #t (list a0 a1 a2) (condition ys ys*)))
        (let* ([a0  (cauchy 0 0.1)]
               [a1  (* -0.5 (log (random)))]
               [ys  (generate-exponential a0 a1 (const xs))])
          (list #f (list a0 a1) (condition ys ys*)))))
  
  (define B (set-list bools universe trues))
  )

#;; Test: Box-Muller method for generating standard normal samples
(begin
  (interval-max-splits 3)
  
  (define/drbayes e
    (let* ([x  (normal/box-muller)]
           [y  (+ x (normal/box-muller))])
      (list x y)))
  
  (define B (set-list reals (real-set 1.8 2.2)))
  
  (rect-alpha-scale 1/2)
  )

#;; Test: arithmetic
(begin
  (interval-max-splits 4)
  
  (define/drbayes e
    (let* ([x  (random-std-cauchy)]
           [y  (random-std-cauchy)])
      (list x y (* x y))))
  
  (define B (set-list reals reals (real-set -0.1 0.2))))

#;; Test: sqr
;; Preimage should look like the graph of the function
(begin
  (interval-max-splits 1)
  (define/drbayes e
    (let ([x  (uniform -1 1)]
          [y  (uniform 0 1)])
      (list x y (- y (sqr x)))))
  (define B (set-list reals reals (real-set -0.1 0.1))))

#;;; Test: sine and cosine restricted to [-π,π]
;; Looked at top-down, the plots should look like the graphs of the functions
(begin
  (interval-max-splits 1)
  (define/drbayes e
    (let ([x  (uniform (const (- pi)) (const pi))]
          [y  (uniform -1.1 1.1)])
      (list x y (- y (partial-cos x)))))
  (define B (set-list reals reals (real-set -0.1 0.1))))
#;
(begin
  (interval-max-splits 1)
  (define/drbayes e
    (let ([x  (uniform (const (- pi)) (const pi))]
          [y  (uniform -1.1 1.1)])
      (list x y (- y (partial-sin x)))))
  (define B (set-list reals reals (real-set -0.1 0.1))))

#;; Test: asin and acos
;; Looked at top-down, the plots should look like the graphs of the functions
(begin
  (interval-max-splits 1)
  (define/drbayes e
    (let ([x  (uniform -1 1)]
          [y  (uniform 0 (const pi))])
      (list x y (- y (acos x)))))
  (define B (set-list reals reals (real-set -0.1 0.1))))
#;
(begin
  (interval-max-splits 1)
  (define/drbayes e
    (let ([x  (uniform -1 1)]
          [y  (uniform (const (* -0.5 pi)) (const (* 0.5 pi)))])
      (list x y (- y (asin x)))))
  (define B (set-list reals reals (real-set -0.1 0.1))))

#;; Test: Normal-Normal model
;; Preimage should be a banana shape
(begin
  (interval-max-splits 3)
  ;(interval-min-length (expt 0.5 1.0))
  
  (define/drbayes e
    (let* ([x  (normal 0 1)]
           [y1  (normal x 1)]
           [y2  (normal x 1)])
      (list x y1 y2)))
  (define B (set-list reals (real-set -1.2 -0.8) (real-set 1.99 2.01)))
  (normal-normal/lw 0 1 '(2.0) '(1.0)))

#;; Test: Normal-Normal model
;; Preimage should be a banana shape
(begin
  (interval-max-splits 0)
  ;(interval-min-length (expt 0.5 1.0))
  
  (define/drbayes e
    (let* ([x  (normal 0 1)]
           [y  (normal x 1)])
      (list x (and (<= 1.9 y) (<= y 2.1)))))
  (define B (set-list reals trues))
  (normal-normal/lw 0 1 '(2.0) '(1.0)))

#;; Test: Normal-Normal with first variable floored
(begin
  (interval-max-splits 2)
  (define/drbayes e
    (let* ([x  (normal 0 8)]
           [y  (normal (floor x) 1)])
      (list x y)))
  (define B (set-list reals (real-set -0.1 0.1))))

#;
(begin
  (interval-max-splits 2)
  (define/drbayes e
    (let ([x  (random)]
          [y  (random)])
      (list x y (- x y))))
  (define B (set-list reals reals (real-set -0.05 0.05))))

#;; Test: thermometer that goes to 100
(begin
  (interval-max-splits 5)
  (interval-min-length 0.0)
  (define e
    (drbayes
     (let* ([x  (normal 90 10)]
            [y  (+ x (normal 0 1))])
       (list x
             (strict-if (y . > . 100) 100 (strict-if (y . < . 0) 0 y))
             ;(if (y . > . 100) 100 y)
             ))))
  
  (define B (set-list reals (real-set 100.0 100.0))))

#;; Test: Normal-Normal model with circular condition
;; Preimage should look like a football set up for a field goal
(begin
  (interval-max-splits 3)
  
  (define/drbayes (mysqr x)
    (if (negative? x) (sqr x) (sqr x)))
  
  (define/drbayes (hypot x y)
    (sqrt (+ (sqr x) (sqr y))))
  
  (define/drbayes e
    (let* ([x0  (normal 0 1)]
           [x1  (normal x0 1)])
      (list x0 x1 (hypot x0 x1))))
  
  (define B (set-list reals reals (real-set 0.95 1.05))))

#;
(begin
  (interval-max-splits 3)
  
  (define/drbayes (hypot x y)
    (sqrt (+ (sqr x) (sqr y))))
  
  (define/drbayes e
    (let* ([x0  (normal 0 1)]
           [x1  (normal x0 1)]
           [z   (hypot x0 x1)])
      (list x0 x1 (and (z . > . 0.95) (z . < . 1.05)))))
  
  (define B (set-list reals
                      reals
                      trues)))

#;; Test: random square, with obviously repeated variable in condition
(begin
  (interval-max-splits 2)
  
  (define/drbayes e
    (let* ([x0  (random)]
           [x1  (random)])
      (list x0 x1 (sqrt (+ (sqr x0) (sqr (+ x0 x1)))))))
  
  (define B (set-list reals reals (real-set 0.99 1.01))))

#;; Test: Normal-Normal or Cauchy-Cauchy, depending on random variable
(begin
  (interval-max-splits 4)
  (define/drbayes e
    (if
     ;((random) . < . (const #i499/1000))
     (boolean (const #i499/1000))
     (let ([x  (random-std-normal)])
       (list x (normal x 1)))
     (let ([x  (random-std-cauchy)])
       (list x (cauchy x 1)))))
  (define B (set-list reals (real-set 0.9 1.1))))

#;; Test: Boolean(p) distribution
;; Preimage should be [0,p); sampler should fail only once
(begin
  (define p #i2/5)
  (define e (drbayes (if (boolean (const p)) #t #f)))
  (define B trues))

#;; Test: List length distributed Geometric(0.5)
(begin
  (interval-max-splits 2)
  
  (define/drbayes (geom)
    (if (boolean (const 0.5)) (cons #t (geom)) null))
  
  (define e (drbayes (geom)))
  (define B universe))

#;; Test: constrained Geometric(p) distribution
(begin
  (interval-max-splits 1)
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

#;; Test: Conditioning on Geometric(0.5) distribution defined via recursion
;; Image points should lie on integer x coordinates and be clustered around 3
(begin
  (interval-max-splits 1)
  
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

#;; Test: Normal-Normal model with more observations
;; Density plot, mean, and stddev should be similar to those produced by `normal-normal/lw'
(begin
  (interval-max-splits 0)
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

#;; Test: failure on a branch of lazy if
;; Preimage should be a lower right triangle; shouldn't have many failures
(begin
  (interval-max-splits 2)
  (define/drbayes e
    (let ([x  (random)]
          [y  (random)]
          [z  (random)])
      (if (x . < . y) (fail) z)))
  (define B universe))

#;; Test: tagging
;; Preimage should be [0,1]
(begin
  (define t0 (make-set-tag 't0))
  (define e (drbayes (tag (random) t0)))
  (define B (set-tag reals t0)))

#;; Test: tagging and untagging
;; Preimage should be [0,1]
(begin
  (define t0 (make-set-tag 't0))
  (define e (drbayes (untag (tag (random) t0) t0)))
  (define B reals))

#;; Test: Normal-Normal with circular condition and first variable tagged
(begin
  (define t0 (make-set-tag 't0))
  (define/drbayes e
    (let* ([x0  (tag (random-std-normal) t0)]
           [x1  (normal (untag x0 t0) 1)])
      (list (untag x0 t0) x1 (sqrt (+ (sqr (untag x0 t0)) (sqr x1))))))
  (define B (set-list reals reals (real-set 0.95 1.05))))

#;; Test: Normal-Normal with circular condition and first variable tagged with probability 0.5
(begin
  (interval-max-splits 0)
  (define t0 (make-set-tag 't0))
  (define/drbayes e
    (let* ([x0  (if ((random) . < . 0.45)
                    (tag (normal 0 1) t0)
                    (random-std-normal))]
           [y0  (if (tag? x0 t0) (untag x0 t0) x0)]
           [x1  (normal y0 1)])
      (list y0 x1 (sqrt (+ (sqr y0) (sqr x1))))))
  (define B (set-list reals reals (real-set 0.95 1.05))))

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
           (loop (rest orig-samples))]
          #;
          [_
           (match-define (cons R T) S)
           (define r (omega-set-sample-point R))
           (define t (trace-set-sample-point T))
           (define s (cons r t))
           (define b (bottom (delay "refinement-sample-point failed")))
           (cons (domain-sample S s b m p 0.0 0.0) (loop (rest orig-samples)))])]))))

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

;(plot-z-ticks no-ticks)
#;
(plot3d (list (rectangles3d (append*
                             (map (λ: ([d : domain-sample])
                                    (omega-rect->plot-rects (car (domain-sample-S d))))
                                  not-samples))
                            #:alpha (min 1.0 (* all-alpha (rect-alpha-scale)))
                            #:color 1 #:line-color 1)
              (rectangles3d (append*
                             (map (λ: ([d : domain-sample])
                                    (omega-rect->plot-rects (car (domain-sample-S d))))
                                  samples))
                            #:alpha (min 1.0 (* all-alpha (rect-alpha-scale)))
                            #:color 3 #:line-color 3))
        #:x-min 0 #:x-max 1 #:y-min 0 #:y-max 1 #:z-min 0 #:z-max 1
        #:x-label "ω0" #:y-label "ω1"
        #:angle 0
        #:altitude 90
        #:out-file "aaa-rects.pdf")

(: domain-sample->omega-point (domain-sample -> (Listof Flonum)))
(define (domain-sample->omega-point d)
  (omega->point (car (domain-sample-s d))))

(plot3d (list (points3d (map domain-sample->omega-point samples)
                        #:sym 'dot #:size 12
                        #:alpha (* all-alpha (point-alpha-scale))
                        #:color 3 #:fill-color 3
                        ;#:label "ω ∈ preimage g B"
                        )
              (points3d (map domain-sample->omega-point not-samples)
                        #:sym 'times #:size 5
                        #:alpha (* all-alpha (point-alpha-scale))
                        #:color 1 #:fill-color 1
                        ;#:label "ω ∉ preimage g B"
                        ))
        #:x-min 0 #:x-max 1 #:y-min 0 #:y-max 1 #:z-min 0 #:z-max 1
        #:x-label "ω0" #:y-label "ω1" #:z-label "ω2"
        ;#:angle 0 #:altitude 90
        #:out-file "aaa-points.pdf")

(plot3d (points3d (sample (discrete-dist (map domain-sample->omega-point samples) ws)
                          num-samples)
                  #:sym 'dot #:size 12 #:alpha alpha)
        #:x-min 0 #:x-max 1 #:y-min 0 #:y-max 1 #:z-min 0 #:z-max 1
        #:x-label "ω0" #:y-label "ω1"
        #:angle 0
        #:altitude 90)

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
  (plot (list (points (sample (discrete-dist xss ws) num-samples)
                      #:sym 'dot #:size 48 #:color "white" #:alpha (min 1 (* 2 alpha)))
              (points (sample (discrete-dist xss ws) num-samples)
                      #:sym 'dot #:size 12 #:alpha alpha))
        #:x-label "x" #:y-label "y"
        ;#:angle 0
        ;#:altitude 90
        #:out-file "aaa-image-points.pdf"
        ))

(define x0s (map (inst first Flonum Flonum) xss))

(with-handlers ([exn?  (λ (_) (printf "weight density plot failed~n"))])
  (plot (density ws) #:x-label "weight" #:y-label "density"))

(with-handlers ([exn?  (λ (_) (printf "weight/measure scatter plot failed~n"))])
  (plot (points (map (λ: ([w : Flonum] [m : Flonum]) (list w m)) ws ms)
                #:sym 'dot #:size 12 #:alpha alpha)
        #:x-label "weight" #:y-label "measure"))

(printf "Corr(W,M) = ~v~n" (correlation ws ms))

(with-handlers ([exn?  (λ (_) (printf "density plot failed~n"))])
  (plot (list (function (distribution-pdf (normal-dist 2/3 (sqrt 1/3)))
                        #:width 3.5 #:color 0 #:style 'short-dash
                        #:label "True Density")
              (density x0s 1 ws
                       #:width 4 #:color "white")
              (density x0s 1 ws
                       #:width 2
                       #:label "Est. Density"
                       ))
        #:x-label "x"
        #:y-label "density"
        #:out-file "aaa-density.pdf"))

(printf "E[x0] = ~v~n" (mean x0s (ann ws (Sequenceof Real))))
(printf "sd[x0] = ~v~n" (stddev x0s (ann ws (Sequenceof Real))))

#lang typed/racket/base

(require math/bigfloat)

(provide
 bigfloat->bfprob
 bfprob->bigfloat
 bfprob?
 bfprob-normalize
 bfprob1-
 bfprob*
 bfprob/
 bfprob+
 bfprob-
 bfprob-midpoint
 )

(define-values (-fllog2.bf fllog2.bf)
  (parameterize ([bf-precision 53])
    (define fllog2.bf (bf (log 2.0)))
    (values (bf- fllog2.bf) fllog2.bf)))

;; ===================================================================================================
;; Conversion

(: bigfloat->bfprob (-> Bigfloat Bigfloat))
(define (bigfloat->bfprob x)
  (if (bf<= x (bf 0.5))
      (bflog x)
      (bf- (bflog1p (bf- x)))))

(: bfprob-not-nan? (-> Bigfloat Boolean))
(define (bfprob-not-nan? x)
  (or (bf<= x -fllog2.bf)
      (bf< fllog2.bf x)))

(: bfprob? (-> Bigfloat Boolean))
(define (bfprob? x)
  (or (bfprob-not-nan? x)
      (bfnan? x)))

(: bfprob->bigfloat (-> Bigfloat Bigfloat))
(define (bfprob->bigfloat x)
  (cond [(not (bfprob-not-nan? x))  +nan.bf]
        [(bfnegative? x)  (bfexp x)]
        [else  (bf- (bfexpm1 (bf- x)))]))

;; ===================================================================================================
;; Normalization: put any nonzero symmetric log probability in the range [-∞,-log(2)] ∪ (log(2),∞]

(: bfprob-normalize (-> Bigfloat Bigfloat))
(define (bfprob-normalize x)
  (cond [(bfprob? x)  x]
        [(bfzero? x)  +inf.bf]
        [(bfnegative? x)  (bf- (bflog (bf- (bfexpm1 x))))]
        [else             (bflog (bf- (bfexpm1 (bf- x))))]))

;; ===================================================================================================
;; Complement

(: bfprob1- (-> Bigfloat Bigfloat))
(define (bfprob1- x)
  (cond [(or (bf< x -fllog2.bf) (bf< fllog2.bf x))  (bf- x)]
        [(bf= x -fllog2.bf)  x]
        [else  +nan.bf]))

;; ===================================================================================================
;; Multiplication

(: bfprob* (-> Bigfloat Bigfloat Bigfloat))
(define (bfprob* x y)
  (bfprob-normalize
   (if (not (and (bfprob-not-nan? x) (bfprob-not-nan? y)))
       +nan.bf
       (let ([x  (bfmax x y)]
             [y  (bfmin x y)])
         (cond [(bf= x +inf.bf)  y]  ; Multiplying by 1.0
               [(bf= y -inf.bf)  y]  ; Multiplying by 0.0
               ;; Quadrant III
               [(bfnegative? x)  (bf+ x y)]
               ;; Quadrant II/IV
               [(bfnegative? y)  (bf+ y (bflog1p (bf- (bfexp (bf- x)))))]
               ;; Quadrant I
               [else
                (define z (bf+ (bflog1p (bf- (bfexp (bf- y))))
                               (bflog1p (bf- (bfexp (bf- x))))))
                (cond [(bf<= z -fllog2.bf)  z]
                      [(bf< x (bf 2048))
                       (bf- (bf+ x y) (bflog (bf+ (bfexpm1 x) (bfexp y))))]
                      [else
                       ;; See flprob*/error for rationale
                       (bf- y (bflog1p (bfexp (bf- y x))))])])))))

;; ===================================================================================================
;; Division

(: bfprob/ (-> Bigfloat Bigfloat Bigfloat))
(define (bfprob/ x y)
  (bfprob-normalize
   (cond [(not (and (bfprob-not-nan? x) (bfprob-not-nan? y)))  +nan.bf]
         [(bf< y x)  +nan.bf]        ; Result is > 1
         [(bf= y -inf.bf)  +nan.bf]  ; Dividing by 0
         [(bf= x -inf.bf)  x]        ; Dividing 0
         [(bf= x y)  +inf.bf]        ; Dividing equal numbers
         ;; Quadrant III
         [(bfnegative? y)
          (cond [(bfnegative? x)
                 (define z (bf- x y))
                 (if (bf< z (bflog (bf 0.5))) z (bf- (bflog1p (bf- (bfexp z)))))]
                [else
                 +nan.bf])]
         ;; Quadrant II
         [(bfnegative? x)
          (define z (bf- x (bflog1p (bf- (bfexp (bf- y))))))
          (if (bf<= z -fllog2.bf)
              z
              (bf- (bflog1p (bf- (bfexp z)))))]
         ;; Quadrant I
         [(or (bf< y (bf 2048)) (bf> (bf- x y) (bf -2048)))
          (bf+ (bf- x y) (bflog (bf- (bf/ (bfexpm1 y) (bfexpm1 (bf- x y))))))]
         [else  x])))  ; See flprob//error for rationale

;; ===================================================================================================
;; Addition

(: bfprob+ (-> Bigfloat Bigfloat Bigfloat))
(define (bfprob+ x y)
  (bfprob-normalize
   (cond [(not (and (bfprob-not-nan? x) (bfprob-not-nan? y)))  +nan.bf]
         [(bf> x (bf- y))  +nan.bf]  ; Result > 1
         [(bf= x (bf- y))  +inf.bf]  ; Result = 1
         [else
          (let ([x  (bfmax x y)]
                [y  (bfmin x y)])
            (cond [(bf= x -inf.bf)  -inf.bf]  ; Result 0 + 0 = 0
                  ;; Quadrant III
                  [(bfnegative? x)
                   (define z (bf+ x (bflog1p (bfexp (bf- y x)))))
                   (if (bf<= z -fllog2.bf)
                       z
                       (bf- (bflog1p (bf- (bf+ (bfexp x) (bfexp y))))))]
                  ;; Quadrants II and IV
                  [else
                   (bf- x (bflog1p (bf- (bfexp (bf+ y x)))))]))])))

;; ===================================================================================================
;; Subtraction

(: bfprob- (-> Bigfloat Bigfloat Bigfloat))
(define (bfprob- x y)
  (cond [(not (and (bfprob-not-nan? x) (bfprob-not-nan? y)))  +nan.bf]
        [(bf< x y)  +nan.bf]             ; Result would be negative
        [(bf= x y)  -inf.bf]             ; Subtracting same number
        [(bf= x +inf.bf)  (bfprob1- y)]  ; Subtracting from 1
        [(bf= y -inf.bf)  x]             ; Subtracting 0
        ;; Quadrant III
        [(bfnegative? x)
         (bf+ x (bflog (bf- (bfexpm1 (bf- y x)))))]
        ;; Quadrant IV
        [(bfnegative? y)
         (define z (bflog (bfmax 0.bf (bf- (bf- (bfexpm1 (bf- x))) (bfexp y)))))
         (if (bf<= z -fllog2.bf)
             z
             (let ([x  (bfmax (bf- x) y)]
                   [y  (bfmin (bf- x) y)])
               (bf- (bf+ x (bflog1p (bfexp (bf- y x)))))))]
        ;; Quadrant I
        [else
         (bf- (bflog (bf- (bfexpm1 (bf- y x)))) y)]))

;; ===================================================================================================
;; Midpoint

(: bfprob-midpoint (-> Bigfloat Bigfloat Bigfloat))
(define (bfprob-midpoint x y)
  (cond
    [(not (and (bfprob-not-nan? x) (bfprob-not-nan? y)))  +nan.bf]
    [else
     (bfprob-normalize
      (let loop ([x x] [y y])
        (cond [(bf> x (bf- y))  (bf- (loop (bf- x) (bf- y)))]
              [(bf= x y)  x]
              [(bf= x (bf- y))  log2.bf]
              [else
               (let ([x  (bfmax x y)]
                     [y  (bfmin x y)])
                 (cond [(bfnegative? x)
                        (bf+ (bf- log2.bf) (bf+ x (bflog1p (bfexp (bf- y x)))))]
                       [else
                        (bf+ (bf- log2.bf) (bflog1p (bf- (bfexp y) (bfexp (bf- x)))))]))])))]))

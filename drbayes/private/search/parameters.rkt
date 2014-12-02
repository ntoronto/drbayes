#lang typed/racket/base

(require racket/flonum
         "../set.rkt"
         "../flonum.rkt")

(provide (all-defined-out))

(: ->prob (-> Nonempty-Prob-Set (-> (U Flonum Prob) Prob)))
(define ((->prob X) x)
  (define p (if (prob? x) x (flonum->prob x)))
  (cond [(and (prob? p) (prob-set-member? X p))  p]
        [else  (raise-argument-error '->
                                     (format "Flonum or Prob in ~v" X)
                                     x)]))

;; ===================================================================================================
;; General parameters

(: drbayes-refinement-search? (Parameterof Boolean))
(: drbayes-refinement-sample? (Parameterof Boolean))
(: drbayes-refinement-axis-prob-min (Parameterof (U Flonum Prob) Prob))
(: drbayes-refinement-prob-min (Parameterof (U Flonum Prob) Prob))

(define drbayes-refinement-search? (make-parameter #t))
(define drbayes-refinement-sample? (make-parameter #t))
(define drbayes-refinement-axis-prob-min (make-parameter prob-0 (->prob probs)))
(define drbayes-refinement-prob-min (make-parameter prob-0 (->prob probs)))

;; ===================================================================================================
;; Sampler parameters

(: drbayes-sample-axis-prob-min (Parameterof (U Flonum Prob) Prob))

(define drbayes-sample-axis-prob-min
  (make-parameter (Prob (fllog #i1/32)) (->prob (Plain-Prob-Interval prob-0 prob-1 #f #t))))

;; ===================================================================================================
;; Enumerator parameters

(: drbayes-enumerate-prob-min (Parameterof (U Flonum Prob) Prob))
(: drbayes-enumerate-relative-prob-min (Parameterof (U Flonum Prob) Prob))

(define drbayes-enumerate-prob-min
  (make-parameter prob-1 (->prob (Plain-Prob-Interval prob-0 prob-1 #f #t))))

(define drbayes-enumerate-relative-prob-min
  (make-parameter (Prob (fllog #i1/1024)) (->prob (Plain-Prob-Interval prob-0 prob-1 #f #t))))

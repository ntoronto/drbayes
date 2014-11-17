#lang typed/racket

(require drbayes
         plot/typed
         (only-in typed/rackunit check-exn)
         "abstract-float.rkt")

;; Buggy version: inexact 1-p is in the badlands of log, so relative error is unbounded
(define/drbayes (flgeom u p)
  (fl/ (fllog u)
       (fllog (fl- (fl 1) p))))

;; Fixed version
(define/drbayes (flgeom* u p)
  (fl/ (fllog u)
       (fllog1p (flneg p))))

(: verify (-> meaning Natural Any))
(define (verify expr n)
  (define xyzs (let-values ([(xyzs _)  (drbayes-sample expr n)])
                 (cast xyzs (Listof (List Flonum Flonum Flonum)))))
  (printf "~v points~n" (length xyzs))
  (plot3d (points3d xyzs #:sym 'point)
          #:title "Inputs vs. error in epsilons"
          #:x-min 0 #:x-max 1
          #:y-min 0 #:y-max 1))

(printf "Verifying flgeom...~n")
;; Apply flgeom to exact inputs and keep samples only when error is greater than 3 epsilons
(verify (drbayes (let* ([x  (random)]
                        [y  (random)]
                        [ze  (flgeom (fl x) (fl y))])
                   (strict-if (> (float-error ze) (const (* 3 epsilon.f)))
                              (list x y (/ (float-error ze) (const epsilon.f)))
                              (fail))))
        2000)

(printf "Verifying flgeom*~n")
;; Same as above, but the drbayes exception "cannot sample from the empty set" means there are no
;; inputs that cause error > 1.51 epsilons
(check-exn
 exn?
 (λ ()
   (verify (drbayes (let* ([x  (random)]
                           [y  (random)]
                           [ze  (flgeom* (fl x) (fl y))])
                      (strict-if (> (float-error ze) (const (* 1.51 epsilon.f)))
                                 (list x y (/ (float-error ze) (const epsilon.f)))
                                 (fail))))
           1000))
 "cannot sample from the empty set")

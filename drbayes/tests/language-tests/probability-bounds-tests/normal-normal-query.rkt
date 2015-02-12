#lang racket

(require racket/flonum
         drbayes
         drbayes/private/flonum
         drbayes/tests/test-utils
         plot)

(define (print-hline) (printf "~n~a~n" (make-string 90 #\=)))

(print-hline)

(define 3d-plots? (make-parameter #t))

;; Normal-normal model
(begin
  (3d-plots? #f)
  
  (drbayes-enumerate-prob-min (flexpt 2.0 -2.0))
  (drbayes-enumerate-relative-prob-min (flexpt 2.0 -8.0))
  
  (define/drbayes (e)
    (let* ([x   (normal 0 1)]
           [y0  (normal x 1)])
      (< 0.9 y0 1.1)))
  )

#;; Normal-normals model
(begin
  (drbayes-enumerate-prob-min 1.0)
  (drbayes-enumerate-relative-prob-min (flexpt 2.0 -6.0))
  
  (define/drbayes (e)
    (let* ([x   (normal 0 1)]
           [y0  (normal x 1)]
           [y1  (normal x 1)])
      (and (< 0.9 y0 1.1)
           (< 1.3 y1 1.5))))
  )

(define-values (p-min p-mid p-max)
  (drbayes-query (drbayes (e))))

(print-hline)

(printf "~nPr[true] ∈ [~v, ~v]~n" (prob->flonum p-min) (prob->flonum p-max))
(printf "Pr[true] ≈ ~v~n" (prob->flonum p-mid))

(print-hline)

(define t-Sss (drbayes-query-t-stores))
(define f-Sss (drbayes-query-f-stores))

(plot-width 300)
(plot-height 300)
(plot-font-size 8)

(for ([t-Ss  (in-list t-Sss)]
      [f-Ss  (in-list f-Sss)]
      [i  (in-naturals)])
  
  (cond [(zero? (modulo i 2))
         (printf "~nIteration ~v~nAfter refinement:~n" (quotient i 2))]
        [else
         (printf "After interior culling:~n")])
  
  (cond [(3d-plots?)
         (printf
          "~a~a~n"
          (plot3d (rectangles3d (append* (map store-set->plot-rects t-Ss)) #:alpha 0.5)
                  #:x-min 0 #:x-max 1
                  #:y-min 0 #:y-max 1
                  #:z-min 0 #:z-max 1
                  #:x-label "ω0" #:y-label "ω1" #:z-label "ω2"
                  #:title "Appx. f⁻¹({true}) boundary")
          (plot3d (rectangles3d (append* (map store-set->plot-rects f-Ss)) #:alpha 0.5)
                  #:x-min 0 #:x-max 1
                  #:y-min 0 #:y-max 1
                  #:z-min 0 #:z-max 1
                  #:x-label "ω0" #:y-label "ω1" #:z-label "ω2"
                  #:title "Appx. f⁻¹({false}) boundary"))]
        [else
         (printf
          "~a~a~n"
          (plot (rectangles (append* (map store-set->plot-rects t-Ss)) #:alpha 0.5)
                #:x-min 0 #:x-max 1
                #:y-min 0 #:y-max 1
                #:x-label "ω0" #:y-label "ω1"
                #:title "Appx. f⁻¹({true}) boundary")
          (plot (rectangles (append* (map store-set->plot-rects f-Ss)) #:alpha 0.5)
                #:x-min 0 #:x-max 1
                #:y-min 0 #:y-max 1
                #:x-label "ω0" #:y-label "ω1"
                #:title "Appx. f⁻¹({false}) boundary"))]))

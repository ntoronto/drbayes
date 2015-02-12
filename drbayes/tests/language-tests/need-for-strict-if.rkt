#lang typed/racket

(require drbayes)

(define/drbayes (e1)
  (if (< 0.5 (random)) 1 (fail)))

(define/drbayes (e2)
  (if (< 0.5 (random))
      (if (< 0.5 (random))
          1
          (fail))
      (fail)))

(define/drbayes (e3)
  (let ([b  (if (< 0.5 (random)) #t #f)])
    (if b 1 (if (not b) (fail) 2))))

(define/drbayes (geom)
  (if (< 0.5 (random)) 0 (+ 1 (geom))))

(: do-test (-> meaning Any))
(define (do-test e)
  (for*/list : (Listof Any) ([s?  '(#t #f)]
                             [p : Natural  '(0 1 2 3 4)])
    (define-values (xs ws) 
      (parameterize ([drbayes-refinement-search?  s?]
                     [drbayes-sample-max-splits  p])
        (drbayes-sample e 50)))
    (list s? p (length xs))))

(do-test (drbayes (e1)))
(do-test (drbayes (e2)))
(do-test (drbayes (e3)))
(do-test (drbayes (geom)))

#|
(define/drbayes (e)
  (let ([x  (list (list #t #t #t (< (random) 0.5))
                  (list #t #t #t (< (random) 0.5))
                  (list #t #t #t (< (random) 0.5)))])
    (;strict-if
     if
     (equal? x (list (list #t #t #t #f)
                     (list #t #t #t #t)
                     (list #t #t #t #t)))
     x
     (fail))))

(for ([n  '(10 100 1000 2000)])
  (displayln
   (for/list ([p  '(#i1 #i1/2 #i1/4 #i1/8 #i1/16)])
     (parameterize ([drbayes-refinement-search-prob-min  p])
       (let-values ([(xs ws)  (drbayes-sample (drbayes (e)) n)])
         (length xs))))))
|#

#lang typed/racket

(require math/distributions
         math/statistics
         plot/typed)

(provide normal-normal/lw)

(: normal-normal/lw (Real Real (Listof Real) (Listof Real) -> Void))
(define (normal-normal/lw m s ys ss)
  (define d0 (normal-dist m s))
  (define xs (sample d0 100000))
  (define ws (map (λ: ([x : Flonum])
                    (apply * (map (λ: ([y : Real] [s : Real])
                                    (pdf (normal-dist x s) y))
                                  ys ss)))
                  xs))

  (define resampled-xs (sample (discrete-dist xs ws) 100000))
  
  (print (plot (density resampled-xs)))
  (newline)
  (printf "mean = ~v~n" (mean resampled-xs))
  (printf "stddev = ~v~n" (stddev resampled-xs)))

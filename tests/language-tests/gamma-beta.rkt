#lang typed/racket

(require "../../main.rkt")

(provide (all-defined-out))

(define/drbayes (gamma/exp α)
  (if (> α 0)
      (- (gamma/exp (- α 1))
         (log (random)))
      0))

(define/drbayes (gamma/norm α)
  (let ([x  (normal α (sqrt α))])
    (if (positive? x) x (gamma/norm α))))

(define/drbayes (gamma α)
  (if (> α 5)
      (gamma/norm α)
      (gamma/exp α)))

(define/drbayes (beta α β)
  (let ([x  (gamma α)]
        [y  (gamma β)])
    (/ (+ 1 (/ y x)))))

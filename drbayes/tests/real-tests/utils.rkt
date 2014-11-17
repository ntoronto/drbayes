#lang typed/racket/base

(require racket/list
         math/flonum)

(provide (all-defined-out))

(: flonum-between (-> Flonum Flonum Flonum))
(define (flonum-between a b)
  (let ([c  (* 0.5 (+ a b))])
    (if (< a c b)
        c
        (let ([c  (+ (* 0.5 a) (* 0.5 b))])
          (if (< a c b)
              c
              (let ([c  (ordinal->flonum (quotient (+ (flonum->ordinal a) (flonum->ordinal b)) 2))])
                (if (<= a c b)
                    c
                    +nan.0)))))))

(: generate-domain-values (-> Flonum Flonum (Listof Flonum)))
(define (generate-domain-values mn mx)
  (let* ([seeds  (list mn mx (flonum-between mn mx))]
         [seeds  (if (< mn 0.0 mx) (cons 0.0 seeds) seeds)])
    (sort
     (remove-duplicates
      (append* (map (λ ([a : Flonum])
                      (define as (build-list 11 (λ ([i : Index])
                                                  (let ([a  (flstep a (- i 5))])
                                                    (min mx (max mn a))))))
                      (if (zero? a) (cons -0.0 as) as))
                    seeds)))
     fl<)))

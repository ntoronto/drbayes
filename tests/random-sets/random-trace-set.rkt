#lang typed/racket

(require "../private/set/bool-set.rkt"
         "../private/set/tree-set.rkt"
         "random-bool-set.rkt")

(provide random-nonempty-trace-set random-trace-set)

(: random-nonempty-trace-set (-> Nonempty-Trace-Set))
(define (random-nonempty-trace-set)
  (let loop ([n 4])
    (define r (random))
    (cond [(or (r . < . 0.4) (zero? n))  traces]
          [else
           (let reject ()
             (define I (random-bool-set))
             (cond [(or (empty-bool-set? I) (bools? I))  (reject)]
                   [else
                    (trace-set I (trace-children-set (loop (- n 1)) (loop (- n 1))))]))])))

(: random-trace-set (-> Trace-Set))
(define (random-trace-set)
  (cond [((random) . < . 0.1)  empty-trace-set]
        [else  (random-nonempty-trace-set)]))

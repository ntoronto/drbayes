#lang racket

(require "../../main.rkt")

(define/drbayes (thing)
  (let ([x  (list (list #t #t #t (< (random) 0.5))
                  (list #t #t #t (< (random) 0.5))
                  (list #t #t #t (< (random) 0.5)))])
    (strict-if
     ;if
     (equal? x (list (list #t #t #t #f)
                     (list #t #t #t #t)
                     (list #t #t #t #t)))
     x
     (fail))))

(for/list ([splits  (in-range 5)])
  (parameterize ([interval-max-splits  splits])
    (let-values ([(xs ws)  (drbayes-sample (drbayes (thing)) 100)])
      (length xs))))

(for/list ([splits  (in-range 5)])
  (parameterize ([interval-max-splits  splits])
    (let-values ([(xs ws)  (drbayes-sample (drbayes (thing)) 1000)])
      (length xs))))

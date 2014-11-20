#lang typed/racket

(require typed/rackunit
         drbayes/private/set
         drbayes/private/arrow)

(check-true
 (set-equal? (range/pre (run/pre (+/pre) (set-pair (real-set 0.0 1.0 #t #t)
                                                   (real-set 0.0 1.0 #t #t))))
             (real-set 0.0 2.0 #t #t)))

(check-true
 (set-equal? (preimage/pre (run/pre (+/pre) (set-pair (real-set 0.0 1.0 #t #t)
                                                      (real-set 0.0 1.0 #t #t)))
                           (real-set 0.0 0.5 #t #t))
             (set-pair (real-set 0.0 0.5 #t #t)
                       (real-set 0.0 0.5 #t #t))))

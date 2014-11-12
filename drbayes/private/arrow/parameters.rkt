#lang typed/racket/base

(require racket/performance-hint)

(provide (all-defined-out))

(begin-encourage-inline
  
  (: drbayes-always-terminate? (Parameterof Boolean))
  (define drbayes-always-terminate? (make-parameter #f))
  
  (define (make-drbayes-cache-procs)
    (: cache? (Parameterof Boolean))
    (: check-bad-misses? (Parameterof Boolean))
    (: hits (Boxof Natural))
    (: misses (Boxof Natural))
    (: bad-miss-hash (Boxof (HashTable Any Natural)))
    
    (define cache? (make-parameter #t))
    (define check-bad-misses? (make-parameter #f))
    (define hits (box 0))
    (define misses (box 0))
    (define bad-miss-hash (box ((inst make-hash Any Natural))))
    
    (: register-hit! (-> Void))
    (define (register-hit!)
      (set-box! hits (+ 1 (unbox hits))))
    
    (: register-miss! (-> Void))
    (define (register-miss!)
      (set-box! misses (+ 1 (unbox misses))))
    
    (: register-bad-miss! (-> Any Void))
    (define (register-bad-miss! key)
      (define hash (unbox bad-miss-hash))
      (hash-set! hash key (+ 1 (hash-ref hash key (λ () 0)))))
    
    (: stats (-> (Values Natural Natural (HashTable Any Natural))))
    (define (stats)
      (values (unbox hits)
              (unbox misses)
              (make-immutable-hash (hash-map (unbox bad-miss-hash)
                                             (inst cons Any Natural)))))
    
    (: stats-clear! (-> Void))
    (define (stats-clear!)
      (set-box! hits 0)
      (set-box! misses 0)
      (set-box! bad-miss-hash ((inst make-hash Any Natural))))
    
    (values cache?
            check-bad-misses?
            register-hit!
            register-miss!
            register-bad-miss!
            stats
            stats-clear!))
  
  (define-values (drbayes-image-cache?
                  drbayes-image-cache-check-bad-misses?
                  register-image-cache-hit!
                  register-image-cache-miss!
                  register-image-cache-bad-miss!
                  drbayes-image-cache-stats
                  drbayes-image-cache-stats-clear!)
    (make-drbayes-cache-procs))
  
  (define-values (drbayes-pair-cache?
                  drbayes-pair-cache-check-bad-misses?
                  register-pair-cache-hit!
                  register-pair-cache-miss!
                  register-pair-cache-bad-miss!
                  drbayes-pair-cache-stats
                  drbayes-pair-cache-stats-clear!)
    (make-drbayes-cache-procs))
  
  (define-values (drbayes-preimage-cache?
                  drbayes-preimage-cache-check-bad-misses?
                  register-preimage-cache-hit!
                  register-preimage-cache-miss!
                  register-preimage-cache-bad-miss!
                  drbayes-preimage-cache-stats
                  drbayes-preimage-cache-stats-clear!)
    (make-drbayes-cache-procs))
  
  )  ; begin-encourage-inline

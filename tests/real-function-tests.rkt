#lang typed/racket

(require "../private/arrow.rkt"
         "../private/set.rkt"
         (rename-in "random-real-set.rkt" [random-real :random-real])
         "random-bool-set.rkt"
         "rackunit-utils.rkt")

(: random-nonempty-real-set (-> Nonempty-Real-Set))
(define (random-nonempty-real-set)
  (define A (random-real-set))
  (if (empty-real-set? A) (random-nonempty-real-set) A))

(: random-nonempty-real-pair-set (-> Nonempty-Set))
(define (random-nonempty-real-pair-set)
  (set-pair (random-nonempty-real-set)
            (random-nonempty-real-set)))

(: random-nonempty-bool-set (-> Nonempty-Bool-Set))
(define (random-nonempty-bool-set)
  (define A (random-bool-set))
  (if (empty-bool-set? A) (random-nonempty-bool-set) A))

(: random-real (Set -> Flonum))
(define (random-real A)
  (:random-real (set-take-reals A)))

(: random-real-pair (Set -> (Pair Flonum Flonum)))
(define (random-real-pair A)
  (cons (random-real (set-proj-fst A))
        (random-real (set-proj-snd A))))

(: test-soundness (Symbol Bot-Arrow Pre-Arrow
                          (-> Nonempty-Set)
                          (-> Nonempty-Set)
                          (Nonempty-Set -> Value)
                          Integer -> Void))
(define (test-soundness name f h random-domain-set random-range-set random-domain-value n)
  (printf "Testing ~a~n" name)
  (time
   (for: ([_  (in-range n)])
     (define X (random-domain-set))
     (define h* (h X))
     (define Y (range/pre h*))
     (define a (random-domain-value X))
     (define b (f a))
     
     (unless (bottom? b)
       (check-prop (set-member? Y b)
                   (format "~a: image failed: A = ~v; Y = ~v; a = ~v; b = ~v" name X Y a b)))
     
     (define B (random-range-set))
     (define A (ap/pre h* B))
     
     (unless (bottom? b)
       (check-prop (implies (set-member? B b) (set-member? A a))
                   (format "~a: preimage failed: X = ~v; Y = ~v; B = ~v; A = ~v; a = ~v; b = ~v"
                           name X Y B A a b)))))
  (newline))

(: test-unary-op (Symbol Bot-Arrow Pre-Arrow Integer -> Void))
(define (test-unary-op name f h n)
  (test-soundness name f h random-nonempty-real-set random-nonempty-real-set random-real n))

(: test-unary-pred (Symbol Bot-Arrow Pre-Arrow Integer -> Void))
(define (test-unary-pred name f h n)
  (test-soundness name f h random-nonempty-real-set random-nonempty-bool-set random-real n))

(: test-binary-op (Symbol Bot-Arrow Pre-Arrow Integer -> Void))
(define (test-binary-op name f h n)
  (test-soundness name f h random-nonempty-real-pair-set random-nonempty-real-set random-real-pair n))

(: test-binary-pred (Symbol Bot-Arrow Pre-Arrow Integer -> Void))
(define (test-binary-pred name f h n)
  (test-soundness name f h random-nonempty-real-pair-set random-nonempty-bool-set random-real-pair n))

(define n 50000)

(test-unary-op 'neg neg/bot neg/pre n)
(test-unary-op 'exp exp/bot exp/pre n)
(test-unary-op 'log log/bot log/pre n)
(test-unary-op 'sqrt sqrt/bot sqrt/pre n)
(test-unary-op 'asin asin/bot asin/pre n)
(test-unary-op 'acos acos/bot acos/pre n)
(test-unary-op 'floor floor/bot floor/pre n)
(test-unary-op 'ceiling ceiling/bot ceiling/pre n)
(test-unary-op 'round round/bot round/pre n)
(test-unary-op 'truncate truncate/bot truncate/pre n)
(test-unary-op 'cauchy cauchy/bot cauchy/pre n)
(test-unary-op 'normal normal/bot normal/pre n)
(test-unary-op 'abs abs/bot abs/pre n)
(test-unary-op 'sqr sqr/bot sqr/pre n)
(test-unary-op 'recip recip/bot recip/pre n)

(test-unary-pred 'negative? negative?/bot negative?/pre n)
(test-unary-pred 'positive? positive?/bot positive?/pre n)
(test-unary-pred 'nonpositive? nonpositive?/bot nonpositive?/pre n)
(test-unary-pred 'nonnegative? nonnegative?/bot nonnegative?/pre n)

(test-binary-op '+ +/bot +/pre n)
(test-binary-op '- -/bot -/pre n)
(test-binary-op '* */bot */pre n)
(test-binary-op '/ //bot //pre n)

(test-binary-pred '< </bot </pre n)
(test-binary-pred '> >/bot >/pre n)
(test-binary-pred '<= <=/bot <=/pre n)
(test-binary-pred '>= >=/bot >=/pre n)

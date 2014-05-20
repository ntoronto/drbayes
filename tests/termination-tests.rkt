#lang typed/racket

(require typed/rackunit
         "../private/arrow.rkt"
         "../private/set.rkt")

;; ===================================================================================================
;; Bottom and bottom* arrows

(: halt-on-true/bot Bot-Arrow)
(define halt-on-true/bot
  (ifte/bot id/bot id/bot (lazy/bot (λ () halt-on-true/bot))))

(: halt-on-true/bot* Bot*-Arrow)
(define halt-on-true/bot*
  (ifte/bot* id/bot* id/bot* (lazy/bot* (λ () halt-on-true/bot*))))

(: halt-on-true*/bot* Bot*-Arrow)
(define halt-on-true*/bot*
  (ifte*/bot* id/bot* id/bot* (lazy/bot* (λ () halt-on-true*/bot*))))

(: random-domain-cons (Value -> (Pair (Pair Omega Trace) Value)))
(define (random-domain-cons a)
  (cons (cons (random-omega) (random-trace)) a))

(check-equal? (halt-on-true/bot #t) #t)

(check-equal? ((run/bot* halt-on-true/bot* '()) (random-domain-cons #t)) #t)

(define f (run/bot* halt-on-true*/bot* '()))

(check-equal? (remove-duplicates
               (filter (compose not bottom?)
                       (build-list 100 (λ: ([_ : Index]) (f (random-domain-cons #t))))))
              '(#t))

(check-equal? (remove-duplicates
               (filter (compose not bottom?)
                       (build-list 100 (λ: ([_ : Index]) (f (random-domain-cons #f))))))
              '())

;; ===================================================================================================
;; Preimage and preimage* arrows

(: halt-on-true/pre Pre-Arrow)
(define halt-on-true/pre
  (ifte/pre id/pre id/pre (lazy/pre (λ () halt-on-true/pre))))

(: halt-on-true/pre* Pre*-Arrow)
(define halt-on-true/pre*
  (ifte/pre* id/pre* id/pre* (lazy/pre* (λ () halt-on-true/pre*))))

(: halt-on-true*/pre* Pre*-Arrow)
(define halt-on-true*/pre*
  (ifte*/pre* id/pre* id/pre* (lazy/pre* (λ () halt-on-true*/pre*))))

(: random-domain-pair (Nonempty-Set -> Nonempty-Set))
(define (random-domain-pair A)
  (set-pair (set-pair omegas traces) A))

(check-equal? (ap/pre (halt-on-true/pre trues) trues)
              trues)

(check-equal? (ap/pre (halt-on-true/pre trues) falses)
              empty-set)

(check-equal? (ap/pre ((run/pre* halt-on-true/pre* '()) (set-pair (set-pair omegas traces) trues))
                      trues)
              (set-pair (set-pair omegas traces) trues))

(check-equal? (ap/pre ((run/pre* halt-on-true/pre* '()) (set-pair (set-pair omegas traces) trues))
                      falses)
              empty-set)

(define h (run/pre* halt-on-true*/pre* '()))

(define true-traces (trace-set-unproj traces '() trues))
(define false-traces (trace-set-unproj traces '() falses))
(define domain (set-pair (set-pair omegas traces) bools))

;; ---------------------------------------------------------------------------------------------------
;; Tests with unconstrained traces

;; Change to #t when Pre*-Arrow combinators are implemented as in Toronto & McCarthy 2014:
(define paper-preimage*-arrow? #f)
;; Otherwise, leave #f because there are 9 tests below (guarded by a `when') that terminate only
;; using that preimage* arrow

(printf "bbb~n")
(check-equal? (ap/pre (h (set-pair (set-pair omegas traces) bools)) bools)
              domain)

(printf "bbt~n")
(check-equal? (ap/pre (h (set-pair (set-pair omegas traces) bools)) trues)
              domain)

(printf "bbf~n")
(check-equal? (ap/pre (h (set-pair (set-pair omegas traces) bools)) falses)
              domain)

(printf "btb~n")
(check-equal? (ap/pre (h (set-pair (set-pair omegas traces) trues)) bools)
              (set-pair (set-pair omegas true-traces) trues))

(printf "btt~n")
(check-equal? (ap/pre (h (set-pair (set-pair omegas traces) trues)) trues)
              (set-pair (set-pair omegas true-traces) trues))

(printf "btf~n")
(check-equal? (ap/pre (h (set-pair (set-pair omegas traces) trues)) falses)
              (if paper-preimage*-arrow?
                  (set-pair (set-pair omegas true-traces) trues)
                  empty-set))

(printf "bfb~n")
(when paper-preimage*-arrow?
  (check-equal? (ap/pre (h (set-pair (set-pair omegas traces) falses)) bools)
                (set-pair (set-pair omegas false-traces) falses)))

(printf "bft~n")
(when paper-preimage*-arrow?
  (check-equal? (ap/pre (h (set-pair (set-pair omegas traces) falses)) trues)
                (set-pair (set-pair omegas false-traces) falses)))

(printf "bff~n")
(when paper-preimage*-arrow?
  (check-equal? (ap/pre (h (set-pair (set-pair omegas traces) falses)) falses)
                (set-pair (set-pair omegas false-traces) falses)))

;; ---------------------------------------------------------------------------------------------------
;; Tests with traces that force the true branch

(printf "tbb~n")
(check-equal? (ap/pre (h (set-pair (set-pair omegas true-traces) bools)) bools)
              (set-pair (set-pair omegas true-traces) trues))

(printf "tbt~n")
(check-equal? (ap/pre (h (set-pair (set-pair omegas true-traces) bools)) trues)
              (set-pair (set-pair omegas true-traces) trues))

(printf "tbf~n")
(check-equal? (ap/pre (h (set-pair (set-pair omegas true-traces) bools)) falses)
              empty-set)

(printf "ttb~n")
(check-equal? (ap/pre (h (set-pair (set-pair omegas true-traces) trues)) bools)
              (set-pair (set-pair omegas true-traces) trues))

(printf "ttt~n")
(check-equal? (ap/pre (h (set-pair (set-pair omegas true-traces) trues)) trues)
              (set-pair (set-pair omegas true-traces) trues))

(printf "ttf~n")
(check-equal? (ap/pre (h (set-pair (set-pair omegas true-traces) trues)) falses)
              empty-set)

(printf "tfb~n")
(check-equal? (ap/pre (h (set-pair (set-pair omegas true-traces) falses)) bools)
              empty-set)

(printf "tft~n")
(check-equal? (ap/pre (h (set-pair (set-pair omegas true-traces) falses)) trues)
              empty-set)

(printf "tff~n")
(check-equal? (ap/pre (h (set-pair (set-pair omegas true-traces) falses)) falses)
              empty-set)

;; ---------------------------------------------------------------------------------------------------
;; Tests with traces that force the false branch

(printf "fbb~n")
(when paper-preimage*-arrow?
  (check-equal? (ap/pre (h (set-pair (set-pair omegas false-traces) bools)) bools)
                (set-pair (set-pair omegas
                                    (bot-basic (trace-set-unproj false-traces '(1 1) falses)))
                          falses)))

(printf "fbt~n")
(when paper-preimage*-arrow?
  (check-equal? (ap/pre (h (set-pair (set-pair omegas false-traces) bools)) trues)
                (set-pair (set-pair omegas
                                    (bot-basic (trace-set-unproj false-traces '(1 1) falses)))
                          falses)))

(printf "fbf~n")
(when paper-preimage*-arrow?
  (check-equal? (ap/pre (h (set-pair (set-pair omegas false-traces) bools)) falses)
                (set-pair (set-pair omegas
                                    (bot-basic (trace-set-unproj false-traces '(1 1) falses)))
                          falses)))

(printf "ftb~n")
(check-equal? (ap/pre (h (set-pair (set-pair omegas false-traces) trues)) bools)
              empty-set)

(printf "ftt~n")
(check-equal? (ap/pre (h (set-pair (set-pair omegas false-traces) trues)) trues)
              empty-set)

(printf "ftf~n")
(check-equal? (ap/pre (h (set-pair (set-pair omegas false-traces) trues)) falses)
              empty-set)

(printf "ffb~n")
(when paper-preimage*-arrow?
  (check-equal? (ap/pre (h (set-pair (set-pair omegas false-traces) falses)) bools)
                (set-pair (set-pair omegas
                                    (bot-basic (trace-set-unproj false-traces '(1 1) falses)))
                          falses)))

(printf "fft~n")
(when paper-preimage*-arrow?
  (check-equal? (ap/pre (h (set-pair (set-pair omegas false-traces) falses)) trues)
                (set-pair (set-pair omegas
                                    (bot-basic (trace-set-unproj false-traces '(1 1) falses)))
                          falses)))

(printf "fff~n")
(when paper-preimage*-arrow?
  (check-equal? (ap/pre (h (set-pair (set-pair omegas false-traces) falses)) falses)
                (set-pair (set-pair omegas
                                    (bot-basic (trace-set-unproj false-traces '(1 1) falses)))
                          falses)))

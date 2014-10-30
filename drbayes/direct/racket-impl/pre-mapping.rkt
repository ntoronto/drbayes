#lang typed/racket/base

(require racket/match
         "set.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Preimage mappings

(struct: pre-mapping ([range : Set] [fun : (Set -> Set)]) #:transparent)

(define pre-range pre-mapping-range)

(define empty-pre-mapping
  (pre-mapping empty-set (λ (_) empty-set)))

(: pre-ap (pre-mapping Set -> Set))
(define (pre-ap h B)
  (match-define (pre-mapping Y p) h)
  (p (set-meet B Y)))

(: pre-pair (pre-mapping pre-mapping -> pre-mapping))
(define (pre-pair h1 h2)
  (match-define (pre-mapping Y1 p1) h1)
  (match-define (pre-mapping Y2 p2) h2)
  (pre-mapping (set-prod Y1 Y2)
               (λ: ([B : Set])
                 (set-meet (p1 (set-proj-fst B)) (p2 (set-proj-snd B))))))

(: pre-comp (pre-mapping pre-mapping -> pre-mapping))
(define (pre-comp h2 h1)
  (match-define (pre-mapping Z p2) h2)
  (pre-mapping Z (λ: ([C : Set]) (pre-ap h1 (p2 C)))))

(: pre-plus (pre-mapping pre-mapping -> pre-mapping))
(define (pre-plus h1 h2)
  (pre-mapping (set-join (pre-range h1) (pre-range h2))
               (λ: ([B : Set]) (set-join (pre-ap h1 B) (pre-ap h2 B)))))

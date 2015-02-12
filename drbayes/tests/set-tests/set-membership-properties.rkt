#lang typed/racket

(require typed/rackunit
         "../test-utils.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Property checks for lattices with membership

(: check-member (All (T X) ((T X -> Boolean) T X -> Any)))
(define (check-member member? A x)
  (check-prop (member? A x) (format "~a: membership failed on ~v ~v" member? A x)))

(: check-subseteq (All (T X) ((T X -> Boolean) (T T -> Boolean) T X T X -> Any)))
(define (check-subseteq member? subseteq? A x B y)
  (when (subseteq? A B)
    (for: ([x  (in-list (list x y))])
      (check-prop (implies (member? A x) (member? B x))
                  (format "~a ~a: subseteq membership failed on ~v ~v ~v" member? subseteq? A B x))
      (check-prop (implies (not (member? B x)) (not (member? A x)))
                  (format "~a ~a: subseteq non-membership failed on ~v ~v ~v"
                          member? subseteq? A B x)))))

(: check-join (All (T X) ((T X -> Boolean) (T T -> (Values T Boolean)) T X T X -> Any)))
(define (check-join member? join A x B y)
  (define-values (C C-exact?) (join A B))
  (for: ([x  (in-list (list x y))])
    (check-prop (implies (or (member? A x) (member? B x)) (member? C x))
                (format "~a ~a: join membership failed on ~v ~v ~v" member? join A B x))
    (when C-exact?
      (check-prop (implies (not (or (member? A x) (member? B x))) (not (member? C x)))
                  (format "~a ~a: join non-membership failed on ~v ~v ~v" member? join A B x)))))

(: check-meet (All (T X) ((T X -> Boolean) (T T -> (Values T Boolean)) T X T X -> Any)))
(define (check-meet member? meet A x B y)
  (define-values (C C-exact?) (meet A B))
  (for: ([x  (in-list (list x y))])
    (check-prop (implies (and (member? A x) (member? B x)) (member? C x))
                (format "~a ~a: meet membership failed on ~v ~v ~v" member? meet A B x))
    (when C-exact?
      (check-prop (implies (not (and (member? A x) (member? B x))) (not (member? C x)))
                  (format "~a ~a: meet non-membership failed on ~v ~v ~v" member? meet A B x)))))

;; ===================================================================================================
;; All membership lattice checks

(: check-membership-lattice (All (T X) ((T -> Boolean)
                                        (T X -> Boolean)
                                        (T T -> Boolean)
                                        (T T -> (Values T Boolean))
                                        (T T -> (Values T Boolean))
                                        (-> T)
                                        (T -> X) -> Any)))
(define (check-membership-lattice empty? member? subseteq? join meet random-set random-member)
  (define A (random-set))
  (define B (random-set))
  
  ;(printf "A = ~v~n" A)
  ;(printf "B = ~v~n" B)

   (when (and (not (empty? A)) (not (empty? B)))
     (define x (random-member A))
     (define y (random-member B))
     
     ;(printf "x = ~v~n" x)
     ;(printf "y = ~v~n" y)
     
     (check-member member? A x)
     (check-member member? B y)
     
     (check-subseteq member? subseteq? A x B y)
     
     (check-join member? join A x B y)
     (check-meet member? meet A x B y)))

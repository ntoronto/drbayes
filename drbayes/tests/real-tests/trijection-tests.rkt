#lang typed/racket/base

(require racket/match
         typed/rackunit
         math/flonum
         "../../private/set.rkt"
         "../../private/real.rkt"
         "utils.rkt")

(: border-function (-> Boolean Boolean Flonum Flonum Flonum Flonum Flonum Flonum
                       (-> Flonum Flonum Flonum)))
(define ((border-function inc1? inc2? a-min a-max b-min b-max c-min c-max) a b)
  (cond [(and (fl> a a-min) (fl< a a-max) (fl> b b-min) (fl< b b-max))  +nan.0]
        [(or (fl< a a-min) (fl> a a-max) (fl< b b-min) (fl> b b-max))  +nan.0]
        [else
         (let-values ([(a-min a-max)  (if inc1? (values a-min a-max) (values a-max a-min))]
                      [(b-min b-max)  (if inc2? (values b-min b-max) (values b-max b-min))])
           (define a-min? (fl= a a-min))
           (define a-max? (fl= a a-max))
           (define b-min? (fl= b b-min))
           (define b-max? (fl= b b-max))
           (cond [a-min?  (if b-max? +nan.0 c-min)]
                 [b-min?  (if a-max? +nan.0 c-min)]
                 [a-max?  c-max]
                 [b-max?  c-max]
                 [else  +nan.0]))]))

(: check-border (Boolean Boolean (-> Flonum Flonum Flonum)
                         Flonum Flonum Flonum Flonum Flonum Flonum
                         -> Any))
(define (check-border inc1? inc2? f a-min a-max b-min b-max c-min c-max)
  (define g (border-function inc1? inc2? a-min a-max b-min b-max c-min c-max))
  (define as (generate-domain-values a-min a-max))
  (define bs (generate-domain-values b-min b-max))
  ;(printf "    ~a × ~a~n" as bs)
  (for* ([a  (in-list as)]
         [b  (in-list bs)])
    (define c (g a b))
    (unless (flnan? c)
      ;(printf "    ~a ~a~n" a b)
      (check-true (= c (f a b))
                  (format "failed border check: ~a ~v ~v should be ~v, not ~v"
                          f a b c (f a b))))))

(: check-trijection-border (trijection Symbol -> Any))
(define (check-trijection-border f name)
  (printf "Testing trijection ~a~n" name)
  (match-define (trijection inc1? inc2? X Y Z fc/rndd fc/rndu fa/rndd fa/rndu fb/rndd fb/rndu) f)
  (define-values (a-min a-max a-min? a-max?) (real-interval-fields X))
  (define-values (b-min b-max b-min? b-max?) (real-interval-fields Y))
  (define-values (c-min c-max c-min? c-max?) (real-interval-fields Z))
  (define inc3? (not (eq? inc1? inc2?)))
  (printf "  Testing fc/rndd~n")
  (check-border inc1? inc2? fc/rndd a-min a-max b-min b-max c-min c-max)
  (printf "  Testing fc/rndu~n")
  (check-border inc1? inc2? fc/rndu a-min a-max b-min b-max c-min c-max)
  (printf "  Testing fa/rndd~n")
  (check-border inc3? inc1? fa/rndd b-min b-max c-min c-max a-min a-max)
  (printf "  Testing fa/rndu~n")
  (check-border inc3? inc1? fa/rndu b-min b-max c-min c-max a-min a-max)
  (printf "  Testing fb/rndd~n")
  (check-border inc2? inc3? fb/rndd c-min c-max a-min a-max b-min b-max)
  (printf "  Testing fb/rndu~n")
  (check-border inc2? inc3? fb/rndu c-min c-max a-min a-max b-min b-max)
  (newline))

(check-trijection-border trij-add 'trij-add)
(check-trijection-border trij-sub 'trij-sub)

(check-trijection-border trij-mul++ 'trij-mul++)
(check-trijection-border trij-mul+- 'trij-mul+-)
(check-trijection-border trij-mul-+ 'trij-mul-+)
(check-trijection-border trij-mul-- 'trij-mul--)

(check-trijection-border trij-div++ 'trij-div++)
(check-trijection-border trij-div+- 'trij-div+-)
(check-trijection-border trij-div-+ 'trij-div-+)
(check-trijection-border trij-div-- 'trij-div--)

#|
TODO

(check-trijection-border trij-expt++ 'trij-expt++)
(check-trijection-border trij-expt+- 'trij-expt+-)
(check-trijection-border trij-expt-+ 'trij-expt-+)
(check-trijection-border trij-expt-- 'trij-expt--)
|#

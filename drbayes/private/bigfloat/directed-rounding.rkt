#lang typed/racket/base

(require math/bigfloat)

(provide (all-defined-out))

(: bffun/rounding ((Bigfloat -> Bigfloat) (U 'down 'up) -> (Bigfloat -> Bigfloat)))
(define ((bffun/rounding f mode) x)
  (parameterize ([bf-rounding-mode mode]) (f x)))

(: bffun2d/rounding ((Bigfloat Bigfloat -> Bigfloat) (U 'down 'up)
                                                     -> (Bigfloat Bigfloat -> Bigfloat)))
(define ((bffun2d/rounding f mode) x y)
  (parameterize ([bf-rounding-mode mode]) (f x y)))


(define pi/rndu (parameterize ([bf-rounding-mode  'up]) pi.bf))
(define -pi/rndd (bf- pi/rndu))

(define pi/2/rndu (parameterize ([bf-rounding-mode  'up]) (bf/ pi.bf 2.bf)))
(define -pi/2/rndd (bf- pi/2/rndu))

(define gamma-crit/rndd
  (parameterize ([bf-rounding-mode 'down])
    (bf #e1.46163214496836234126265954232572132846819620400644635129598840860)))

(define gamma-crit/rndu
  (parameterize ([bf-rounding-mode 'up])
    (bf #e1.46163214496836234126265954232572132846819620400644635129598840860)))

(define gamma-crit-val/rndd
  (parameterize ([bf-rounding-mode 'down])
    (bf #e0.885603194410888700278815900582588733207951533669903448871200)))

(define gamma-crit-val/rndu
  (parameterize ([bf-rounding-mode 'up])
    (bf #e0.885603194410888700278815900582588733207951533669903448871200)))

(define bfsqr/rndd (bffun/rounding bfsqr 'down))
(define bfsqr/rndu (bffun/rounding bfsqr 'up))

(define bfsqrt/rndd (bffun/rounding bfsqrt 'down))
(define bfsqrt/rndu (bffun/rounding bfsqrt 'up))

(define bfrecip/rndd (bffun/rounding bf/ 'down))
(define bfrecip/rndu (bffun/rounding bf/ 'up))

(define bfexp/rndd (bffun/rounding bfexp 'down))
(define bfexp/rndu (bffun/rounding bfexp 'up))

(define bflog/rndd (bffun/rounding bflog 'down))
(define bflog/rndu (bffun/rounding bflog 'up))

(define bfexpm1/rndd (bffun/rounding bfexpm1 'down))
(define bfexpm1/rndu (bffun/rounding bfexpm1 'up))

(define bflog1p/rndd (bffun/rounding bflog1p 'down))
(define bflog1p/rndu (bffun/rounding bflog1p 'up))

(define bfsin/rndd (bffun/rounding bfsin 'down))
(define bfsin/rndu (bffun/rounding bfsin 'up))

(define bfcos/rndd (bffun/rounding bfcos 'down))
(define bfcos/rndu (bffun/rounding bfcos 'up))

(define bftan/rndd (bffun/rounding bftan 'down))
(define bftan/rndu (bffun/rounding bftan 'up))

(define bfasin/rndd (bffun/rounding bfasin 'down))
(define bfasin/rndu (bffun/rounding bfasin 'up))

(define bfacos/rndd (bffun/rounding bfacos 'down))
(define bfacos/rndu (bffun/rounding bfacos 'up))

(define bfatan/rndd (bffun/rounding bfatan 'down))
(define bfatan/rndu (bffun/rounding bfatan 'up))

(define bfgamma/rndd (bffun/rounding bfgamma 'down))
(define bfgamma/rndu (bffun/rounding bfgamma 'up))

(define bf+/rndd (bffun2d/rounding bf+ 'down))
(define bf+/rndu (bffun2d/rounding bf+ 'up))

(define bf-/rndd (bffun2d/rounding bf- 'down))
(define bf-/rndu (bffun2d/rounding bf- 'up))

(define bf*/rndd (bffun2d/rounding bf* 'down))
(define bf*/rndu (bffun2d/rounding bf* 'up))

(define bf//rndd (bffun2d/rounding bf/ 'down))
(define bf//rndu (bffun2d/rounding bf/ 'up))

(define bfexpt/rndd (bffun2d/rounding bfexpt 'down))
(define bfexpt/rndu (bffun2d/rounding bfexpt 'up))

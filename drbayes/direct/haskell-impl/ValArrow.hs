module ValArrow where

class ValArrow a where
  (->>>) :: a x y -> a y z -> a x z
  (-&&&) :: a x y -> a x z -> a x (y,z)
  valIfte :: a x Bool -> a x y -> a x y -> a x y
  valLazy :: a x y -> a x y
  valId :: a x x
  valConst :: y -> a x y
  valFst :: a (x,y) x
  valSnd :: a (x,y) y


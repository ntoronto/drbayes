module SetArrow where

import Set
import PairSet
import BoolSet

infixr 1 ~>>>
infixr 3 ~&&&

class SetArrow a where
  (~>>>) :: (LatticeSet s1, LatticeSet s2, LatticeSet s3)
            => a s1 s2 -> a s2 s3 -> a s1 s3

  (~&&&) :: (LatticeSet s1, LatticeSet s2, LatticeSet s3)
            => a s1 s2 -> a s1 s3 -> a s1 (PairSet s2 s3)

  setIfte :: (LatticeSet s1, LatticeSet s2)
             => a s1 BoolSet -> a s1 s2 -> a s1 s2 -> a s1 s2

  setLazy :: (LatticeSet s1, LatticeSet s2) => (() -> a s1 s2) -> a s1 s2
  setId :: LatticeSet s1 => a s1 s1
  setConst :: (LatticeSet s1, LatticeSet s2) => (MemberType s2) -> a s1 s2
  setFst :: (LatticeSet s1, LatticeSet s2) => a (PairSet s1 s2) s1
  setSnd :: (LatticeSet s1, LatticeSet s2) => a (PairSet s1 s2) s2


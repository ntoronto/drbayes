module PreMapping where

import Set
import PairSet

-- A type `PreMapping s1 s2' represents preimage mappings with domain `s1' and codomain `s2'.
-- A preimage mapping computes overapproximate preimages of `s2' instances, and has an observable
-- range to 1) ensure correctness; and 2) allow preimage arrow composition.

data PreMapping s1 s2 = PreMapping { preRange :: s2, runPreMapping :: s2 -> s1 }

emptyPreMapping :: (LatticeSet s1, LatticeSet s2) => PreMapping s1 s2
emptyPreMapping = PreMapping empty (\a -> empty)

-- Application computes approximate preimages
preAp :: (LatticeSet s1, LatticeSet s2) => PreMapping s1 s2 -> s2 -> s1
preAp (PreMapping ys p) bs = p (ys /\ bs)

-- Pairing
prePair :: (LatticeSet s1, LatticeSet s2, LatticeSet s3)
           => PreMapping s1 s2 -> PreMapping s1 s3 -> PreMapping s1 (PairSet s2 s3)
prePair (PreMapping ys py) (PreMapping zs pz) =
  PreMapping (prod ys zs) (\bcs -> py (projFst bcs) /\ pz (projSnd bcs))

-- Composition
preComp :: (LatticeSet s1, LatticeSet s2, LatticeSet s3)
           => PreMapping s2 s3 -> PreMapping s1 s2 -> PreMapping s1 s3
preComp (PreMapping zs pz) hy =
  PreMapping zs (\cs -> do preAp hy (pz cs))

-- "Disjoint" union (overapproximation actually makes it a join, but it's a disjoint union operator
-- in the exact semantics)
prePlus :: (LatticeSet s1, LatticeSet s2)
           => PreMapping s1 s2 -> PreMapping s1 s2 -> PreMapping s1 s2
prePlus h1 h2 =
  PreMapping (preRange h1 \/ preRange h2) (\bs -> preAp h1 bs \/ preAp h2 bs)


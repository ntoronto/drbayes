module PreArrow where

import Set
import PairSet
import PreMapping
import SetArrow

-- The preimage arrow: one of the main contributions in Toronto & McCarthy 2014.
-- Instances (over-)approximately run programs backward on sets of outputs.

newtype PreArrow s1 s2 = PreArrow { runPreArrow :: s1 -> PreMapping s1 s2 }

-- Toronto & McCarthy 2014 derives the implementation ultimately from the bottom arrow, and proves
-- that it computes approximate preimages under corresponding bottom arrow computations.

instance SetArrow PreArrow where
  h1 ~>>> h2 =
    PreArrow (\a -> let h1' = runPreArrow h1 a
                        h2' = runPreArrow h2 (preRange h1')
                      in preComp h2' h1')

  h1 ~&&& h2 =
    PreArrow (\a -> prePair (runPreArrow h1 a) (runPreArrow h2 a))

  setIfte h1 h2 h3 =
    PreArrow (\a -> let h1' = runPreArrow h1 a
                        a2 = preAp h1' (singleton True)
                        a3 = preAp h1' (singleton False)
                      in prePlus (runPreArrow h2 a2) (runPreArrow h3 a3))

  setLazy h =
    PreArrow (\a -> if a == empty then emptyPreMapping else runPreArrow (h ()) a)

  setId =
    PreArrow (\a -> PreMapping a (\b -> b))

  setConst y =
    PreArrow (\a -> PreMapping (singleton y) (\b -> if b == empty then empty else a))

  setFst =
    PreArrow (\a -> let a1 = projFst a
                        a2 = projSnd a
                      in PreMapping a1 (\b -> a /\ prod b a2))

  setSnd =
    PreArrow (\a -> let a1 = projFst a
                        a2 = projSnd a
                      in PreMapping a2 (\b -> a /\ prod a1 b))

refine :: (LatticeSet s1, LatticeSet s2) => PreArrow s1 s2 -> s1 -> s2 -> s1
refine h as bs = preAp (runPreArrow h as) bs


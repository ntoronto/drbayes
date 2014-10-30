{-# LANGUAGE
    FlexibleInstances #-}

module PreArrow2 where

import Set
import PairSet
import OrdSet
import MaybeSet
import BoolSet
import TreeSet
import PreMapping
import SetArrow
import PreArrow

-- Another preimage arrow, which can be used as a translation target for probabilistic, partial
-- programs. See Toronto & McCarthy 2014.

type PreArrowArgType' s1 = PairSet (PairSet RSet TSet) s1
newtype PreArrow' s1 s2 =
  PreArrow' { runPreArrow' :: TreeIndex -> PreArrow (PreArrowArgType' s1) s2 }

preArrowLift' :: (LatticeSet s1, LatticeSet s2) => PreArrow s1 s2 -> PreArrow' s1 s2
preArrowLift' h = PreArrow' (\j -> setSnd ~>>> h)

instance SetArrow PreArrow' where
  k1 ~>>> k2 =
    PreArrow' (\j -> setFst ~&&& runPreArrow' k1 (indexLeft j) ~>>> runPreArrow' k2 (indexRight j))

  k1 ~&&& k2 =
    PreArrow' (\j -> runPreArrow' k1 (indexLeft j) ~&&& runPreArrow' k2 (indexRight j))

  setIfte k1 k2 k3 =
    PreArrow' (\j -> setIfte (runPreArrow' k1 (indexLeft j))
                             (runPreArrow' k2 (indexLeft (indexRight j)))
                             (runPreArrow' k3 (indexRight (indexRight j))))

  setLazy k =
    PreArrow' (\j -> setLazy (\() -> (runPreArrow' (k ()) j)))

  setId = preArrowLift' setId
  setConst y = preArrowLift' (setConst y)
  setFst = preArrowLift' setFst
  setSnd = preArrowLift' setSnd

refine' :: (LatticeSet s1, LatticeSet s2) => PreArrow' s1 s2 -> s1 -> s2 -> PreArrowArgType' s1
refine' k as bs = refine (runPreArrow' k j0) (PairSet UnivPairSet as) bs

-- Get the random number at the expression index

setRandom :: TreeIndex -> PreArrow RSet (RealSet)
setRandom j = PreArrow (\a -> PreMapping (project j a /\ unitIvl) (unproject j a))

setRandom' :: LatticeSet s1 => PreArrow' s1 (RealSet)
setRandom' = PreArrow' (\j -> setFst ~>>> setFst ~>>> setRandom j)

-- Get a random boolean

imgLTConst :: RealSet -> Float -> BoolSet
imgLTConst a x =
  let lt = (a /\ preLTConst TrueSet x) /= empty
      gte = (a /\ preLTConst FalseSet x) /= empty
    in if lt
       then if gte then UnivBoolSet else TrueSet
       else if gte then FalseSet else EmptyBoolSet

preLTConst :: BoolSet -> Float -> RealSet
preLTConst EmptyBoolSet x = EmptyIvl
preLTConst UnivBoolSet x = UnivIvl
preLTConst TrueSet x = ivl NegInf (Finite x) False False
preLTConst FalseSet x = ivl (Finite x) PosInf True False

setLTConst :: Float -> PreArrow (RealSet) BoolSet
setLTConst x = PreArrow (\a -> PreMapping (imgLTConst a x) (\b -> a /\ preLTConst b x))

setRandomBool' :: LatticeSet s => Float -> PreArrow' s BoolSet
setRandomBool' p = PreArrow' (\j -> runPreArrow' setRandom' j ~>>> setLTConst p)

-- Index branch traces (not used directly in translations)

setBranch :: TreeIndex -> PreArrow TSet (MaybeSet BoolSet)
setBranch j = PreArrow (\a -> PreMapping (project j a) (unproject j a))

setBranch' :: LatticeSet s1 => PreArrow' s1 (MaybeSet BoolSet)
setBranch' = PreArrow' (\j -> setFst ~>>> setSnd ~>>> setBranch j)

-- A conditional that always converges by never taking more than one branch

setIfte' :: (LatticeSet s1, LatticeSet s2)
            => PreArrow' s1 BoolSet -> PreArrow' s1 s2 -> PreArrow' s1 s2 -> PreArrow' s1 s2
setIfte' k1 k2 k3 = 
  PreArrow'
    (\j ->
      PreArrow
        (\a ->
          let PreMapping ck pk = runPreArrow (runPreArrow' k1 (indexLeft j)) a
              PreMapping cb pb = runPreArrow (runPreArrow' setBranch' j) a
              c = ck /\ withoutNothing cb
              c2 = c /\ singleton True
              c3 = c /\ singleton False
              a2 = pk c2 /\ pb (OnlyJust c2)
              a3 = pk c3 /\ pb (OnlyJust c3)
            in case withoutNothing cb of
                 UnivBoolSet -> PreMapping univ (\b -> a2 \/ a3)
                 TrueSet -> runPreArrow (runPreArrow' k2 (indexLeft (indexRight j))) a2
                 FalseSet -> runPreArrow (runPreArrow' k3 (indexRight (indexRight j))) a3
                 EmptyBoolSet -> emptyPreMapping))
{-
            in if cb == univ
               then PreMapping univ (\b -> a2 \/ a3)
               else prePlus (runPreArrow (runPreArrow' k2 (indexLeft (indexRight j))) a2)
                            (runPreArrow (runPreArrow' k3 (indexRight (indexRight j))) a3)))
-}


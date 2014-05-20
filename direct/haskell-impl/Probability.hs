{-# LANGUAGE
    FlexibleInstances #-}

module Probability where

import Data.List
import Set
import PairSet
import BoolSet
import OrdSet
import TreeSet
import MaybeSet
import PreMapping
import SetArrow
import PreArrow
import PreArrow2
import ValArrow
import BotArrow
import BotArrow2
import TypeSet

class LatticeSet s => SplittableSet s where
  split :: s -> [s]


instance SplittableSet s => SplittableSet (TreeSet s) where
  split (TreeSetNode a l r) =
    do a' <- split a
       l' <- split l
       r' <- split r
       return (treeSetNode a' l' r')

  split EmptyTreeSet = []
  split a = [a]


instance SplittableSet RealSet where
  split (Ivl (Finite a1) (Finite a2) c1 c2) =
    let a = Finite ((a1 + a2) / 2.0)
      in filter (not . (== empty)) [ivl (Finite a1) a c1 True, ivl a (Finite a2) False c2]

  split EmptyIvl = []
  split a = [a]


instance SplittableSet BoolSet where
  split EmptyBoolSet = []
  split UnivBoolSet = [TrueSet, FalseSet]
  split a = [a]

instance SplittableSet s => SplittableSet (MaybeSet s) where
  split (OnlyJust a) = do a' <- split a
                          return (OnlyJust a')
  split a = [a]

instance (SplittableSet s1, SplittableSet s2) => SplittableSet (PairSet s1 s2) where
  split (PairSet a1 a2) = do a1' <- split a1
                             a2' <- split a2
                             return (PairSet a1' a2')
  split EmptyPairSet = []
  split a = [a]

instance SplittableSet (TypeSet x) where
  split EmptyTypeSet = []
  split a = [a]


probability :: (SplittableSet s1, LatticeSet s2) => PreArrow' s1 s2 -> s1 -> s2 -> [Float]
probability k a b = 
  map unionMeasure
      (map (\as -> map (projFst . projFst) as)
           (refinements k a b))

refinements :: (SplittableSet s1, LatticeSet s2) => PreArrow' s1 s2 -> s1 -> s2 -> [[PairSet (PairSet RSet TSet) s1]]
refinements k a b =
  unfoldr (\as -> let as' = map (\a -> preAp (runPreArrow (runPreArrow' k j0) a) b) as
                    in Just (as', concatMap split as'))
          [prod UnivPairSet a]


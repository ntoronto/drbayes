{-# LANGUAGE
    TypeFamilies,
    FlexibleContexts #-}

module Set where

-- A type of class `Set s' denotes members of a bounded lattice of subsets.
-- Members of these subsets are of type `MemberType s'.

infixr 3 /\
infixr 2 \/
infixr 2 \\

class Eq s => LatticeSet s where
  type MemberType s :: *
  empty :: s  -- lattice bottom
  univ :: s   -- lattice top
  (/\) :: s -> s -> s  -- meet
  (\/) :: s -> s -> s  -- join
  member :: s -> MemberType s -> Bool
  singleton :: MemberType s -> s


class LatticeSet s => MeasurableSet s where
  (\\) :: s -> s -> [s]  -- subtract (i.e. relative complement)

union :: MeasurableSet s => [s] -> [s]
union [] = []
union (a:as) = a : union (concatMap (\\ a) as)


class MeasurableSet s => LebesgueMeasurableSet s where
  type MeasureType s :: *
  measure :: s -> MeasureType s

unionMeasure :: (LebesgueMeasurableSet s, Num (MeasureType s)) => [s] -> MeasureType s
unionMeasure as = sum (map measure (union as))


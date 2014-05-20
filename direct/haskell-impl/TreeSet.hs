{-# LANGUAGE
    TypeFamilies,
    TypeSynonymInstances,
    FlexibleInstances #-}

module TreeSet where

import Set
import OrdSet
import MaybeSet
import BoolSet
import PairSet

-- The `TreeIndex' type denotes indexes into infinite (or unbounded) binary trees. In the
-- approximating semantics, it's used as the domain (i.e. index set) of infinite vectors.

type TreeIndex = [Bool]

indexLeft :: TreeIndex -> TreeIndex
indexLeft j = True : j

indexRight :: TreeIndex -> TreeIndex
indexRight j = False : j

j0 = []

--

-- A type `TreeVal x' represents an infinite vector of `x' with finitely many observable values.

data TreeVal x = AnyTreeVal | TreeValNode !x !(TreeVal x) !(TreeVal x)
  deriving(Show,Eq)

treeValRef :: TreeIndex -> TreeVal x -> x
treeValRef [] (TreeValNode a _ _) = a
treeValRef  (True:j) (TreeValNode _ l _) = treeValRef j l
treeValRef (False:j) (TreeValNode _ _ r) = treeValRef j r
-- The `treeValRef j AnyTreeVal' case is missing because trying to do it should be an error: the
-- answer is indeterminate

--

-- A type of class `Set (TreeSet s)' represents subsets of infinite vectors indexed by `TreeIndex'
-- instances, with each component a `MemberType s' value. Each is a rectangle with no more than
-- finitely many full axes.

-- WARNING: Do not use the `TreeSetNode' constructor! See PairSet.hs for reasons.

-- The strictness of the fields is critical to correct operation. If they weren't strict, trees
-- could be infinite, leading membership and equality tests to diverge. The trees can still
-- represent sets of infinite vectors, however, as long as only finitely many axes are restricted.

data TreeSet s = EmptyTreeSet | UnivTreeSet | TreeSetNode !s !(TreeSet s) !(TreeSet s)
  deriving(Show,Eq)

-- The maximal set on each axis is not necessarily `univ', so each contained type needs to
-- specify its maximal set when used in a tree

treeSetNode :: LatticeSet s => s -> TreeSet s -> TreeSet s -> TreeSet s
treeSetNode a l r =
  if a == empty || l == empty || r == empty
  then EmptyTreeSet
  else if a == univ && l == univ && r == univ
       then UnivTreeSet
       else TreeSetNode a l r

instance LatticeSet s => LatticeSet (TreeSet s) where
  type MemberType (TreeSet s) = TreeVal (MemberType s)

  empty = EmptyTreeSet
  univ = UnivTreeSet

  EmptyTreeSet /\ _ = EmptyTreeSet
  _ /\ EmptyTreeSet = EmptyTreeSet
  UnivTreeSet /\ a = a
  a /\ UnivTreeSet = a
  TreeSetNode xs1 l1 r1 /\ TreeSetNode xs2 l2 r2 =
    treeSetNode (xs1 /\ xs2) (l1 /\ l2) (r1 /\ r2)

  EmptyTreeSet \/ a = a
  a \/ EmptyTreeSet = a
  UnivTreeSet \/ _ = UnivTreeSet
  _ \/ UnivTreeSet = UnivTreeSet
  TreeSetNode xs1 ls1 rs1 \/ TreeSetNode xs2 ls2 rs2 =
    treeSetNode (xs1 \/ xs2) (ls1 \/ ls2) (rs1 \/ rs2)

  member EmptyTreeSet _ = False
  member UnivTreeSet _ = True
  member (TreeSetNode xs ls rs) (TreeValNode x l r) =
    member xs x && member ls l && member rs r
  -- The `member (TreeSetNode _ _ _) AnyTreeVal' case is missing because trying to do it should
  -- be an error: the answer is indeterminate for any non-full axis

  singleton AnyTreeVal = UnivTreeSet
  singleton (TreeValNode x l r) = treeSetNode (singleton x) (singleton l) (singleton r)

-- `project' is like `projFst' and `projSnd', generalized to arbitrary products; equivalently, it
-- retrieves an axis from a rectangular set of vectors

project :: LatticeSet s => TreeIndex -> TreeSet s -> s
project j EmptyTreeSet = empty
project j UnivTreeSet = univ
project [] (TreeSetNode xs _ _) = xs
project  (True:j) (TreeSetNode _ l _) = project j l
project (False:j) (TreeSetNode _ _ r) = project j r

-- `unproject' computes rectangular preimages under projection; equivalently, it's a functional
-- update to an axis in a rectangular set of vectors, with intersecting instead of replacing

unproject :: LatticeSet s => TreeIndex -> TreeSet s -> s -> TreeSet s
unproject j EmptyTreeSet _ = EmptyTreeSet
unproject j UnivTreeSet ys = unproject j (TreeSetNode univ UnivTreeSet UnivTreeSet) ys
unproject [] (TreeSetNode xs ls rs) ys = treeSetNode (xs /\ ys) ls rs
unproject  (True:j) (TreeSetNode xs ls rs) ys = treeSetNode xs (unproject j ls ys) rs
unproject (False:j) (TreeSetNode xs ls rs) ys = treeSetNode xs ls (unproject j rs ys)

--

nodeToProd :: LatticeSet s => TreeSet s -> PairSet s (PairSet (TreeSet s) (TreeSet s))
nodeToProd (TreeSetNode x l r) = PairSet x (PairSet l r)

prodToNode :: LatticeSet s => PairSet s (PairSet (TreeSet s) (TreeSet s)) -> TreeSet s
prodToNode (PairSet x (PairSet l r)) = TreeSetNode x l r

instance MeasurableSet s => MeasurableSet (TreeSet s) where
  EmptyTreeSet \\ _ = []
  a \\ EmptyTreeSet = [a]
  _ \\ UnivTreeSet = []
  UnivTreeSet \\ a = TreeSetNode univ UnivTreeSet UnivTreeSet \\ a
  a \\ b = map prodToNode (nodeToProd a \\ nodeToProd b)


-- Instances of infinite vector sets: for unit intervals (random source) and boolean+bottom sets
-- (branch traces)
type RSet = TreeSet RealSet
type TSet = TreeSet (MaybeSet BoolSet)

unitIvl = closedIvl 0.0 1.0 :: RealSet

instance LebesgueMeasurableSet RSet where
  type MeasureType RSet = Float

  measure EmptyTreeSet = 0.0
  measure UnivTreeSet = 1.0
  measure (TreeSetNode a l r) = 
    let Finite p = measure (a /\ unitIvl)
      in p * measure l * measure r


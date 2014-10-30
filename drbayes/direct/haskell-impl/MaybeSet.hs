{-# LANGUAGE
    TypeFamilies #-}

module MaybeSet where

import Set

-- A type `Set (MaybeSet s)' denotes the same subsets as a type of class `Set s', except the value
-- `Nothing' may be contained in any of them.

data MaybeSet s = OnlyJust s | WithNothing s
  deriving(Show,Eq)

withoutNothing :: LatticeSet s => MaybeSet s -> s
withoutNothing (OnlyJust a) = a
withoutNothing (WithNothing a) = a

instance LatticeSet s => LatticeSet (MaybeSet s) where
  type MemberType (MaybeSet s) = Maybe (MemberType s)

  empty = OnlyJust empty
  univ = WithNothing univ

  OnlyJust a /\ OnlyJust b = OnlyJust (a /\ b)
  OnlyJust a /\ WithNothing b = OnlyJust (a /\ b)
  WithNothing a /\ OnlyJust b = OnlyJust (a /\ b)
  WithNothing a /\ WithNothing b = WithNothing (a /\ b)

  OnlyJust a \/ OnlyJust b = OnlyJust (a \/ b)
  WithNothing a \/ OnlyJust b = WithNothing (a \/ b)
  OnlyJust a \/ WithNothing b = WithNothing (a \/ b)
  WithNothing a \/ WithNothing b = WithNothing (a \/ b)

  member (WithNothing a) Nothing = True
  member (OnlyJust a) Nothing = False
  member (WithNothing a) (Just x) = member a x
  member (OnlyJust a) (Just x) = member a x

  singleton Nothing = WithNothing empty
  singleton (Just a) = OnlyJust (singleton a)


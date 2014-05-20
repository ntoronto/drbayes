{-# LANGUAGE
    TypeFamilies #-}

module TypeSet where

import Set

data TypeSet x = EmptyTypeSet | UnivTypeSet
  deriving(Show,Eq)

instance LatticeSet (TypeSet x) where
  type MemberType (TypeSet x) = x

  empty = EmptyTypeSet
  univ = UnivTypeSet

  EmptyTypeSet /\ EmptyTypeSet = EmptyTypeSet
  EmptyTypeSet /\ UnivTypeSet = EmptyTypeSet
  UnivTypeSet /\ EmptyTypeSet = EmptyTypeSet
  UnivTypeSet /\ UnivTypeSet = UnivTypeSet

  EmptyTypeSet \/ EmptyTypeSet = EmptyTypeSet
  EmptyTypeSet \/ UnivTypeSet = UnivTypeSet
  UnivTypeSet \/ EmptyTypeSet = UnivTypeSet
  UnivTypeSet \/ UnivTypeSet = UnivTypeSet

  member EmptyTypeSet _ = False
  member UnivTypeSet _ = True

  singleton x = UnivTypeSet

type NullSet = TypeSet ()

emptyNullSet :: TypeSet ()
emptyNullSet = EmptyTypeSet

univNullSet :: TypeSet ()
univNullSet = UnivTypeSet


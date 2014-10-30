{-# LANGUAGE
    TypeFamilies,
    FlexibleInstances #-}

module OrdSet where

import Set

data Extended x = NegInf | PosInf | Finite x
  deriving(Show,Eq)

isFinite :: Extended x -> Bool
isFinite (Finite a) = True
isFinite _ = False

instance Ord x => Ord (Extended x) where
  NegInf <= _ = True
  _ <= PosInf = True
  PosInf <= _ = False
  _ <= NegInf = False
  Finite a1 <= Finite a2 = a1 <= a2

-- A type `OrdSet (Extended x)' denotes open, closed, or half-open intervals of type `x'.

-- WARNING: Do not use the `Ivl' constructor! See PairSet.hs for reasons.

data OrdSet x = EmptyIvl | UnivIvl | Ivl (Extended x) (Extended x) Bool Bool
  deriving(Eq)

closedIvl :: Ord x => x -> x -> OrdSet x
closedIvl a1 a2 = finiteIvl a1 a2 True True

finiteIvl :: Ord x => x -> x -> Bool -> Bool -> OrdSet x
finiteIvl a1 a2 c1 c2 = ivl (Finite a1) (Finite a2) c1 c2

ivl :: Ord x => Extended x -> Extended x -> Bool -> Bool -> OrdSet x
ivl NegInf PosInf _ _ = UnivIvl
ivl a1 a2 c1 c2 =
  let c1' = c1 && isFinite a1
      c2' = c2 && isFinite a2
    in if a1 < a2
       then Ivl a1 a2 c1' c2'
       else if a1 > a2
            then EmptyIvl
            else if c1' && c2'
                 then Ivl a1 a2 c1' c2'
                 else EmptyIvl

instance Show x => Show (OrdSet x) where
  showsPrec d EmptyIvl = showString "EmptyIvl"
  showsPrec d UnivIvl = showString "UnivIvl"

  showsPrec d (Ivl (Finite a1) (Finite a2) True True) =
    let app_prec = 10
      in showParen (d > app_prec) $
           showString "closedIvl " . showsPrec (app_prec+1) a1 .
           showString " " . showsPrec (app_prec+1) a2

  showsPrec d (Ivl (Finite a1) (Finite a2) c1 c2) =
    let app_prec = 10
      in showParen (d > app_prec) $
           showString "finiteIvl " . showsPrec (app_prec+1) a1 .
           showString " " . showsPrec (app_prec+1) a2 .
           showString " " . showsPrec (app_prec+1) c1 .
           showString " " . showsPrec (app_prec+1) c2

  showsPrec d (Ivl a1 a2 c1 c2) =
    let app_prec = 10
      in showParen (d > app_prec) $
           showString "ivl " . showsPrec (app_prec+1) a1 .
           showString " " . showsPrec (app_prec+1) a2 .
           showString " " . showsPrec (app_prec+1) c1 .
           showString " " . showsPrec (app_prec+1) c2

instance Ord x => LatticeSet (OrdSet x) where
  type MemberType (OrdSet x) = x

  empty = EmptyIvl
  univ = UnivIvl

  EmptyIvl /\ _ = EmptyIvl
  _ /\ EmptyIvl = EmptyIvl
  UnivIvl /\ a = a
  a /\ UnivIvl = a
  Ivl a1 a2 c1 c2 /\ Ivl b1 b2 d1 d2 =
    let (e1,f1) = if a1 > b1 then (a1,c1) else if a1 < b1 then (b1,d1) else (a1, c1 && d1)
        (e2,f2) = if a2 < b2 then (a2,c2) else if a2 > b2 then (b2,d2) else (a2, c2 && d2)
      in ivl e1 e2 f1 f2

  EmptyIvl \/ a = a
  a \/ EmptyIvl = a
  UnivIvl \/ _ = UnivIvl
  _ \/ UnivIvl = UnivIvl
  Ivl a1 a2 c1 c2 \/ Ivl b1 b2 d1 d2 =
    let (e1,f1) = if a1 < b1 then (a1,c1) else if a1 > b1 then (b1,d1) else (a1, c1 || d1)
        (e2,f2) = if a2 > b2 then (a2,c2) else if a2 < b2 then (b2,d2) else (a2, c2 || d2)
      in ivl e1 e2 f1 f2

  member EmptyIvl _ = False
  member UnivIvl _ = True
  member (Ivl a1 a2 c1 c2) a =
    let a' = Finite a
      in (a1 < a' || (a1 == a' && c1)) && (a' < a2 || (a' == a2 && c2))

  singleton a = closedIvl a a


instance Ord x => MeasurableSet (OrdSet x) where
  EmptyIvl \\ _ = []
  a \\ EmptyIvl = [a]
  _ \\ UnivIvl = []
  UnivIvl \\ a = Ivl NegInf PosInf False False \\ a

  Ivl a1 b1 c1 d1 \\ Ivl a2 b2 c2 d2 =
    if (Ivl a1 b1 c1 d1 /\ Ivl a2 b2 c2 d2) == empty
    then [Ivl a1 b1 c1 d1]  
    else
      if a1 < a2 || (a1 == a2 && c1 && not c2)
      then if b2 < b1 || (b2 == b1 && not d2 && d1)
           -- ======
           --   ==
           then [ivl a1 a2 c1 (not c2), ivl b2 b1 (not d2) d1]
           -- ======           ======
           --    ======   or      ===
           else [ivl a1 a2 c1 (not c2)]
      else if b2 < b1 || (b2 == b1 && not d2 && d1)
           --    ======        ======
           -- ======      or   ===
           then [ivl b2 b1 (not d2) d1]
           --   ==             ===        ===           ======
           -- ======   or   ======   or   ======   or   ======
           else []


instance (Num x, Ord x) => LebesgueMeasurableSet (OrdSet x) where
  type MeasureType (OrdSet x) = Extended x

  measure EmptyIvl = Finite (fromInteger 0)
  measure (Ivl (Finite a1) (Finite a2) _ _) = Finite (a2 - a1)
  measure _ = PosInf


type RealSet = OrdSet Float


import Set
import PairSet
import BoolSet
import OrdSet
import TreeSet
import MaybeSet
import TypeSet
import PreMapping
import SetArrow
import PreArrow
import PreArrow2
import ValArrow
import BotArrow
import BotArrow2
import Probability

t = let t0 = unproject [] UnivTreeSet (closedIvl 0.1 0.9) :: RSet
        t1 = unproject [True] t0 (closedIvl 0.2 0.3)
      in t1

haltOnTrue' :: PreArrow' BoolSet BoolSet
haltOnTrue' = setIfte' setId setId (setLazy (\() -> haltOnTrue'))

haltWithProb' :: Float -> PreArrow' NullSet BoolSet
haltWithProb' p = setIfte' (setRandomBool' p)
                           (setConst True ~>>> haltOnTrue')
                           (setConst False ~>>> haltOnTrue')

geom' :: Float -> PreArrow' NullSet BoolSet
geom' p = setIfte' (setRandomBool' p) (setRandomBool' 0.5) (setLazy (\() -> (geom' p)))

main :: IO ()
main = do
  print t
  print (project [True] t)
  print (project [True,False,False,True] t)

  let i1 = closedIvl 1.0 3.0
      i2 = closedIvl 2 4
    in print (i1 /\ i2, i1 \/ i2)

  let i1 = closedIvl 0 1
      i2 = closedIvl 2 3
    in print (i1 /\ i2, i1 \/ i2)

  let i1 = closedIvl 0 1
      i2 = closedIvl 1 2
    in print (i1 /\ i2, i1 \/ i2)

  let i1 = closedIvl 0 1
      i2 = closedIvl 1 2
    in print (prod i1 i2)

  print (union ([closedIvl 0.0 1.0, closedIvl (-0.5) 0.5, closedIvl 0.25 1.2] :: [RealSet]))

  print (prod (closedIvl 0.0 1.0) (closedIvl 0.0 1.0) \\ prod (closedIvl 0.5 1.5) (closedIvl 0.5 1.5))
  print ((prod (UnivIvl :: RealSet) (closedIvl 0.0 1.0)) \\ (prod (UnivIvl :: RealSet) (closedIvl 0.5 1.5)))

  let f = valFst -&&& valSnd :: BotArrow (Integer,Integer) (Integer,Integer)
    in print (runBotArrow f (1,1))

  let h = setFst ~&&& setSnd :: PreArrow (PairSet (OrdSet Integer) (OrdSet Integer))
                                         (PairSet (OrdSet Integer) (OrdSet Integer))
      g = runPreArrow h (prod (closedIvl 0 2) (closedIvl 0 2))
    in print (preAp g (prod (closedIvl 0 1) (closedIvl 0 2)))

  print "Partial programs"

  let haltOnTrue = valIfte valId valId haltOnTrue :: BotArrow Bool Bool
    in print (runBotArrow haltOnTrue True)

  let haltOnTrue = setIfte setId setId (setLazy (\() -> haltOnTrue)) :: PreArrow BoolSet BoolSet
    in print (refine haltOnTrue TrueSet TrueSet)

  let haltOnTrue = valIfte' valId valId haltOnTrue :: BotArrow' Bool Bool
    in print (runBotArrow (runBotArrow' haltOnTrue j0)
                          ((AnyTreeVal, (TreeValNode (Just True) AnyTreeVal AnyTreeVal)), False),
              runBotArrow (runBotArrow' haltOnTrue j0)
                          ((AnyTreeVal, (TreeValNode (Just True) AnyTreeVal AnyTreeVal)), True))

  print (refine' haltOnTrue' UnivBoolSet UnivBoolSet)

  print (refine (runPreArrow' haltOnTrue' j0) 
                (prod (prod UnivTreeSet (unproject [] UnivTreeSet (OnlyJust TrueSet)))
                      UnivBoolSet)
                UnivBoolSet)

  print (refine (runPreArrow' haltOnTrue' j0) 
                (prod (prod UnivTreeSet (unproject [] UnivTreeSet (OnlyJust FalseSet)))
                      UnivBoolSet)
                UnivBoolSet)

  print "Probabilistic programs"
{-
  print (refine' setRandom' univNullSet (closedIvl 0.0 0.5))
  print (refine' (setRandomBool' 0.25) univNullSet TrueSet)
  print (refine' (haltWithProb' 0.3) univNullSet TrueSet)

  print (refine' (setConst True) univNullSet TrueSet)
  print (unproject [] (UnivTreeSet :: RSet) (closedIvl 0.1 0.2))
  print (preRange (runPreArrow (runPreArrow' (setRandomBool' 0.3 :: PreArrow' NullSet BoolSet) j0)
          (prod (prod (unproject [] UnivTreeSet (closedIvl 0.1 0.2)) UnivTreeSet) univNullSet)))
                               
  print (take 3 (probability (setIfte' (setRandomBool' 0.3) (setConst True) (setConst False)) univNullSet TrueSet))
  print (map unionMeasure
             (map (\as -> map (projFst . projFst) as)
                  (take 4 (refinements (geom' 0.3) univNullSet TrueSet))))
-}
  print (take 5 (probability (geom' 0.3) univNullSet FalseSet))

  print ()


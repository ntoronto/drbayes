module BotArrow2 where

import ValArrow
import BotArrow
import TreeSet

-- Random source values
type RVal = TreeVal Float
-- Branch trace values
type TVal = TreeVal (Maybe Bool)

-- Another bottom arrow, which can be used as a translation target for probabilistic, partial
-- programs. See Toronto & McCarthy 2014.

newtype BotArrow' s1 s2 = BotArrow' { runBotArrow' :: TreeIndex -> BotArrow ((RVal, TVal), s1) s2 }

botArrowLift' :: BotArrow x y -> BotArrow' x y
botArrowLift' f = BotArrow' (\j -> valSnd ->>> f)

instance ValArrow BotArrow' where
  k1 ->>> k2 =
    BotArrow' (\j -> valFst -&&& runBotArrow' k1 (indexLeft j) ->>> runBotArrow' k2 (indexRight j))

  k1 -&&& k2 =
    BotArrow' (\j -> runBotArrow' k1 (indexLeft j) -&&& runBotArrow' k2 (indexRight j))

  valIfte k1 k2 k3 =
    BotArrow' (\j -> valIfte (runBotArrow' k1 (indexLeft j))
                             (runBotArrow' k2 (indexLeft (indexRight j)))
                             (runBotArrow' k3 (indexRight (indexRight j))))

  valLazy k = BotArrow' (\j -> valLazy (runBotArrow' k j))
  valId = botArrowLift' valId
  valConst y = botArrowLift' (valConst y)
  valFst = botArrowLift' valFst
  valSnd = botArrowLift' valSnd

-- For getting the random number at the expression index:
valRandom :: TreeIndex -> BotArrow RVal Float
valRandom j = BotArrow (\a -> Just (treeValRef j a))

valRandom' :: BotArrow' x Float
valRandom' = BotArrow' (\j -> valFst ->>> valFst ->>> valRandom j)

-- For indexing branch traces (not used directly in translations):
valBranch :: TreeIndex -> BotArrow TVal Bool
valBranch j = BotArrow (\a -> treeValRef j a)

valBranch' :: BotArrow' x Bool
valBranch' = BotArrow' (\j -> valFst ->>> valSnd ->>> valBranch j)

agrees :: (Bool,Bool) -> Maybe Bool
agrees (True,True) = Just True
agrees (False,False) = Just False
agrees (_,_) = Nothing

valAgrees = BotArrow agrees

-- A version of valIfte that always takes the branch as specified in the branch trace:

valIfte' :: BotArrow' x Bool -> BotArrow' x y -> BotArrow' x y -> BotArrow' x y
valIfte' k1 k2 k3 = 
  BotArrow'
    (\j ->
      valIfte ((runBotArrow' k1 (indexLeft j) -&&& runBotArrow' valBranch' j) ->>> valAgrees)
              (runBotArrow' k2 (indexLeft (indexRight j)))
              (runBotArrow' k3 (indexRight (indexRight j))))


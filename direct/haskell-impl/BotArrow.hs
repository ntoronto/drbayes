module BotArrow where

import ValArrow

newtype BotArrow x y = BotArrow { runBotArrow :: x -> (Maybe y) }

instance ValArrow BotArrow where
  f1 ->>> f2 =
    BotArrow (\a -> do b <- runBotArrow f1 a; runBotArrow f2 b)

  f1 -&&& f2 =
    BotArrow (\a -> do b <- runBotArrow f1 a; c <- runBotArrow f2 a; return (b,c))

  valIfte f1 f2 f3 =
    BotArrow (\a -> do b <- runBotArrow f1 a; if b then runBotArrow f2 a else runBotArrow f3 a)

  valLazy f = f
  valId = BotArrow (\a -> Just a)
  valConst b = BotArrow (\a -> Just b)
  valFst = BotArrow (\(a,b) -> Just a)
  valSnd = BotArrow (\(a,b) -> Just b)


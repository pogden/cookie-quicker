module Game.CookieClicker.Strategy
( optimallyOrder
) where

import Game.CookieClicker.Model
import Data.Label
import Data.List (minimumBy, nub, delete)
import Data.Function (on)

optimallyOrder :: GameState -> [Action] -> [Action]
optimallyOrder g xs = minimumBy (compare `on`
                                    (get time . (foldl (flip doAction) g)))
                                (eqPerms xs)

eqPerms [] = [[]]
eqPerms xs = [x:xt | x <- nub xs, xt <- eqPerms $ delete x xs]

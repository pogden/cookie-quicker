{-# LANGUAGE TemplateHaskell, TypeOperators #-}
import Control.Category
import Data.Label
import Prelude hiding ((.), id)


data GameState = GameState { _time :: Float
                           , _cookies :: Float
                           , _cursors :: Int
                           , _grandmas :: Int
                           } deriving (Show, Eq)

data Item = Item { _cost :: GameState -> Float
                 , _effect :: GameState -> GameState
                 , _removalEffect :: GameState -> GameState
                 }

mkLabels [''GameState, ''Item]

type Action = GameState -> GameState

cursor = Item { _cost = \g -> 15 * (1.15 ^ (get cursors g))
              , _effect = modify cursors (+1)
              , _removalEffect = modify cursors (flip (-) 1)
              }

grandma = Item { _cost = \g -> 100 * (1.15 ^ (get grandmas g))
               , _effect = modify grandmas (+1)
               , _removalEffect = modify grandmas (flip (-) 1)
               }

            
cps :: GameState -> Float
cps g = 4 --manual clicking
      + 0.1 * (fromIntegral $ get cursors g)
      + 0.5 * (fromIntegral $ get grandmas g)

saveUp :: Float -> GameState -> GameState
saveUp c g 
  | c <= get cookies g = g
  | otherwise          = (set cookies c)
                         $ modify time (+ (c - (get cookies g)) / (cps g)) g 

spend :: Float -> GameState -> GameState
spend c g = modify cookies (\x -> x - c) g

start = GameState { _time = 0, _cookies = 0, _cursors = 0, _grandmas = 0}

buy :: Item -> GameState -> GameState
buy item g = get effect item $ spend (get cost item g) $ saveUp (get cost item g) g

sell :: Item -> GameState -> GameState
sell item g = get removalEffect item $ modify cookies (+(0.5 * get cost item g)) g

playSequence :: [Action] -> GameState
playSequence = foldl (flip ($)) start


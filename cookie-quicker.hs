{-# LANGUAGE TemplateHaskell, TypeOperators #-}
import Control.Category
import Data.Label
import Prelude hiding ((.), id)


data GameState = GameState { _time :: Float  -- seconds
                           , _cookies :: Float
                           , _cursors :: Int
                           , _grandmas :: Int
                           } deriving (Show, Eq)

data Building = Cursor
              | Grandma deriving (Enum, Show)

mkLabels [''GameState]

data Action = Buy Building | Sell Building

count :: Building -> (GameState :-> Int)
count Cursor  = cursors
count Grandma = grandmas

cost :: Building -> GameState -> Float
cost building g = baseCost building * 1.15 ^ (get (count building) g)

baseCost :: Building -> Float
baseCost Cursor  =  15
baseCost Grandma = 100 
            
cps :: GameState -> Float
cps g = 4 --manual clicking
      + 0.1 * (fromIntegral $ get cursors g)
      + 0.5 * (fromIntegral $ get grandmas g)

saveUp :: Float -> GameState -> GameState
saveUp c g 
  | c <= (get cookies g) = g
  | otherwise            = (set cookies c)
                         $ modify time (+ (c - (get cookies g)) / (cps g)) g

start = GameState { _time = 0, _cookies = 0, _cursors = 0, _grandmas = 0}

playSequence :: [Action] -> GameState
playSequence = foldl (flip doAction) start

doAction :: Action -> GameState -> GameState
doAction (Buy  building) g = modify (count building) (+1)
                           $ modify cookies (subtract (cost building g))
                           $ saveUp (cost building g)
                           $ g
doAction (Sell building) g = modify (count building) (subtract 1)
                           $ modify cookies (+ (cost building g))
                           $ g


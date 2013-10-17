{-# LANGUAGE TemplateHaskell, TypeOperators #-}
module Game.CookieClicker.Model where
  
import Control.Category
import Data.Label
import Prelude hiding ((.), id)

data GameState = GameState { _time    :: Float  -- seconds
                           , _cookies :: Float
                             -- Buildings
                           , _cursors              :: Int
                           , _grandmas             :: Int
                           , _farms                :: Int
                           , _factories            :: Int
                           , _mines                :: Int
                           , _shipments            :: Int
                           , _alchemyLabs          :: Int
                           , _portals              :: Int
                           , _timeMachines         :: Int
                           , _antimatterCondensers :: Int
                           } deriving (Show, Eq)

data Building = Cursor
              | Grandma
              | Farm
              | Factory
              | Mine
              | Shipment
              | AlchemyLab
              | Portal
              | TimeMachine
              | AntimatterCondenser
                deriving (Ord, Eq, Enum, Bounded, Show)

mkLabels [''GameState]

data Action = Buy Building | Sell Building deriving (Ord, Eq, Show)

count :: Building -> (GameState :-> Int)
count Cursor              = cursors
count Grandma             = grandmas
count Farm                = farms
count Factory             = factories
count Mine                = mines
count Shipment            = shipments
count AlchemyLab          = alchemyLabs
count Portal              = portals
count TimeMachine         = timeMachines 
count AntimatterCondenser = antimatterCondensers

cost :: Building -> GameState -> Float
cost building g = baseCost building * 1.15 ^ (get (count building) g)

baseCost :: Building -> Float
baseCost Cursor              =         15
baseCost Grandma             =        100 
baseCost Farm                =        500
baseCost Mine                =       3000
baseCost Shipment            =      40000
baseCost AlchemyLab          =     200000
baseCost Portal              =    1666666
baseCost TimeMachine         =  123456789
baseCost AntimatterCondenser = 3999999999
            
cps :: GameState -> Float
cps g = 4 --manual clicking
      +      0.1 * (fromIntegral $ get cursors              g)
      +      0.5 * (fromIntegral $ get grandmas             g)
      +      4   * (fromIntegral $ get farms                g)
      +     10   * (fromIntegral $ get factories            g)
      +     40   * (fromIntegral $ get mines                g)
      +    100   * (fromIntegral $ get shipments            g)
      +    400   * (fromIntegral $ get alchemyLabs          g)
      +   6666   * (fromIntegral $ get portals              g)
      +  98765   * (fromIntegral $ get timeMachines         g)
      + 999999   * (fromIntegral $ get antimatterCondensers g)

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
                           $ modify cookies (+ (cost building g)/2)
                           $ g

options :: GameState -> [Action]
options g = fmap Buy [minBound..]
            ++ fmap Sell (filter (g `hasA`) [minBound..])
              where g `hasA` building = get (count building) g > 0

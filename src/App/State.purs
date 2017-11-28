module App.State (
    Row (..)
  , Column (..)
  , StringColor
  , Grid
  , State (..)
  , init
  , mkGrid
  , defaultGrid
  , cell
  , cell_ 
  ) where

import Prelude

import App.Config (config)
import App.Routes (Route, match)
import Data.Generic (class Generic, gShow)
import Data.Map (Map, fromFoldable, lookup, update)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))

defaultColor = "#a9bdbd"

data Row = RTop | RBottom

data Column = CLeft | CRight

type StringColor = String

type Grid = Map Row (Map Column StringColor)

mkGrid :: StringColor ->StringColor -> StringColor -> StringColor -> Grid
mkGrid tl tr bl br = fromFoldable $ rows
  where 
    rows = [ Tuple RTop (fromFoldable $ mkRow tl tr)
           , Tuple RBottom (fromFoldable $ mkRow bl br)
           ]
    mkRow l r = [Tuple CLeft l , Tuple CRight r ]

defaultGrid :: Grid
defaultGrid = mkGrid defaultColor defaultColor defaultColor defaultColor

cell :: Row -> Column -> Grid -> StringColor
cell r c g = fromMaybe "" (join $ lookup c <$> lookup r g)

cell_ :: Row -> Column -> Grid -> String -> Grid
cell_ r c g v = update (\_ -> update (\ _ -> Just v) c <$> lookup r g ) r g

newtype State = State
  { title :: String
  , route :: Route
  , loaded :: Boolean
  , query :: String
  , status :: Maybe String
  , grid :: Grid
  }

derive instance newtypeState :: Newtype State _

init :: String -> State
init url = State
  { title: config.title
  , route: match url
  , loaded: false
  , query : ""
  , status : Nothing
  , grid: defaultGrid
  }

-- 
-- Instances
-- 

derive instance eqRow :: Eq Row
derive instance ordRow :: Ord Row
derive instance genericRow :: Generic Row
instance showRow :: Show Row where
  show = gShow

derive instance eqColumm :: Eq Column
derive instance orColumm :: Ord Column
derive instance genericColumn :: Generic Column
instance showColumn :: Show Column where
  show = gShow


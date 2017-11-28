module App.Events where

import Prelude

import App.Routes (Route)
import App.State (Column(..), Row(..), State(..), StringColor, cell_, defaultGrid)
import Control.Monad.Aff (attempt)
import Control.Monad.Except (runExcept)
import DOM.Event.KeyboardEvent (eventToKeyboardEvent, key)
import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Data.Argonaut.Core (toObject)
import Data.Array (head, any, find)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.StrMap as SM
import Network.HTTP.Affjax (AJAX, get)
import Pux (EffModel, noEffects, onlyEffects)
import Pux.DOM.Events (DOMEvent, targetValue)

data Event = Load Route
           | QueryChange DOMEvent
           | QueryKeyPress String DOMEvent
           | MakeLuisRequest String
           | ReceiveLUISResponse (Either String LUISResponse)

type Effects fx = (ajax :: AJAX | fx)

foldp :: ∀ fx. Event -> State -> EffModel State Event (Effects fx)
foldp (Load route) (State st) = noEffects $ State st { route = route, loaded = true }
foldp (QueryKeyPress query ev) state = case eventToKeyPressed ev of
    "Enter" -> onlyEffects state [ pure <<< Just $ MakeLuisRequest query]
    _ -> noEffects state
foldp (QueryChange ev) (State st) = noEffects $  State st { query = targetValue ev}
  -- { state: state { username = targetValue ev, effects: [] }
foldp (ReceiveLUISResponse (Left err)) (State st) =
  noEffects $ State st { status = Just $ "Error accesing LUIS Service: " <> show err }
foldp (ReceiveLUISResponse (Right response)) state  =
  noEffects $ fn state response
foldp (MakeLuisRequest query) (State st) = { 
    state: State st { status = Just $ "Fetching.." }
  , effects: [ do
      res <- attempt <<< get $ "https://westus.api.cognitive.microsoft.com/luis/v2.0/apps/e7a7035c-e8d9-4e09-a94a-3f56e1a9f5a1?subscription-key=784d0c51aa224fecb75d45a04fe223d2&q=" <> query <>  "&verbose=true"
      let decode r = decodeJson r.response :: Either String LUISResponse
      let response = either (Left <<< show) decode res
      pure <<< Just $ ReceiveLUISResponse response
    ]
  }

fn :: State -> LUISResponse -> State
fn (State st) (LUISResponse lr) = if containRequiredEntities lr.entities
 then State st{ grid = fromMaybe defaultGrid $ (cell_) <$> (rowEntity lr.entities) <*> (columnEntity lr.entities) <*> (Just st.grid) <*> (colorEntity $ lr.entities)
              , query = ""
              } 
 else State st { status = Just "I couldn't process your instruction properly" }

containRequiredEntities :: Array Entity -> Boolean
containRequiredEntities arr = (any isRowEntity arr) && (any isColumnEntity arr) && (any isColorEntity arr)

-- add AllowEntity type class for Rows and Column, etc
-- use: isNothing row
isRowEntity :: Entity -> Boolean
isRowEntity e = case e of
  (ERow _) -> true
  _ -> false

row :: Entity -> Maybe Row
row e = case e of
  (ERow r) -> Just r
  _ -> Nothing

rowEntity :: Array Entity -> Maybe Row
rowEntity arr = join $ row <$> find isRowEntity arr

isColumnEntity :: Entity -> Boolean
isColumnEntity e = case e of
  (EColumn _) -> true
  _ -> false

columnEntity :: Array Entity -> Maybe Column
columnEntity arr = join $ column <$> find isColumnEntity arr

column :: Entity -> Maybe Column
column e = case e of
  (EColumn c) -> Just c
  _ -> Nothing

isColorEntity :: Entity -> Boolean
isColorEntity e = case e of
  (EColor _)-> true
  _ -> false

colorEntity :: Array Entity -> Maybe StringColor
colorEntity arr = join $ color <$> find isColorEntity arr

color :: Entity -> Maybe StringColor
color e = case e of
  (EColor c) -> Just c
  _ -> Nothing



newtype LUISResponse = LUISResponse { query :: String
                                    , entities :: Array Entity 
                                    }

data Entity = ERow Row | EColumn Column | EColor StringColor

instance decodeLUISResponse:: DecodeJson LUISResponse where
  decodeJson j =  case toObject j of
      Just o -> do
        query <- o .? "query"
        entities <- o .? "entities"
        pure $ LUISResponse { query: query, entities: entities }
      _ -> noparse "LUISResponse"

instance decodeEntity :: DecodeJson Entity where
  decodeJson j = case toObject j of
    Just o -> do
      t <- o .? "type"
      value <- case SM.lookup "resolution" o of
        Just resJ  -> case SM.lookup "values" (fromMaybeJObject resJ) of
          Just _ -> (fromMaybeJObject resJ) .? "values"
          _ -> noparse "Entity"
        _ -> noparse "Entity"
      case t of
        "Row" -> pure <<< ERow <<< toRow <<< emptyOrHead $ value
        "Column" -> pure <<< EColumn <<< toColumn <<< emptyOrHead $ value
        "Color" -> pure <<< EColor <<< emptyOrHead $ value
        _ -> noparse "Entity"
    _ -> noparse "Entity"

    where
      fromMaybeJObject = fromMaybe SM.empty <<< toObject
      emptyOrHead = fromMaybe "" <<< head
      toRow s = case s of
        "0" -> RTop
        _  -> RBottom
      toColumn s = case s of
        "0" -> CLeft
        _  -> CRight

noparse :: ∀ a. String -> Either String a
noparse err = Left $ "No parse: " <> err

eventToKeyPressed :: DOMEvent -> String
eventToKeyPressed ev = either (const "") key $ runExcept $ eventToKeyboardEvent ev


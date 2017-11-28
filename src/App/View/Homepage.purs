module App.View.Homepage where

import Prelude

import App.Events (Event(..))
import App.State (Column(..), Row(..), State(..), cell)
import Control.Bind (discard)
import Data.Function (($), (<<<), const)
import Data.Monoid (mempty)
import Pux.DOM.Events (onChange, onClick, onKeyUp)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (input, div, h1, p, button)
import Text.Smolder.HTML.Attributes (className, href, placeholder, value, style)
import Text.Smolder.Markup ((!), (#!), text)

view :: State -> HTML Event
view (State s) =
  div ! className "container-fluid" $ do
    div ! className "row" $ do
      div ! className "col-md-offset-2 col-md-8" ! style "margin-bottom: 20px;" $ do
        h1 $ text "PUX Demo" ! style "horizontal-alignment: center;"
        input ! className "form-control input-lg" 
              ! placeholder "Give me some color instructions" 
              ! value s.query  
              #! onKeyUp (QueryKeyPress s.query)
              #! onChange QueryChange
    
    div ! className "row" $ do
      div ! className "col-md-6" $ do
        div ! className "cell" ! style ("background-color:" <> cell RTop CLeft s.grid) $ mempty
      div ! className "col-md-6" $ do
        div ! className "cell" ! style ("background-color:" <> cell RTop CRight s.grid) $ mempty

    div ! className "row " $ do
      div ! className "col-md-6" $ do
        div ! className "cell" ! style ("background-color:" <> cell RBottom CLeft s.grid) $ mempty
      div ! className "col-md-6" $ do
        div ! className "cell" ! style ("background-color:" <> cell RBottom CRight s.grid) $ mempty

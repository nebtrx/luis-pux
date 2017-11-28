module App.View.Layout where

import App.Events (Event)
import App.Routes (Route(NotFound, Home))
import App.State (State(..))
import App.View.Homepage as Homepage
import App.View.NotFound as NotFound
import CSS (CSS, fromString, (?), fontSize, display, inlineBlock, margin, marginLeft, px, value, key, color, backgroundColor, padding, borderRadius)
import CSS.Border (border, solid)
import CSS.Geometry (minHeight)
import CSS.Text (textDecoration, noneTextDecoration, letterSpacing)
import CSS.Text.Transform (textTransform, uppercase)
import CSS.TextAlign (center, textAlign)
import Color (fromHexString, rgb, rgba)
import Control.Bind (discard)
import Data.Function (($), (#))
import Data.Maybe (fromMaybe)
import Pux.DOM.HTML (HTML, style)
import Text.Smolder.HTML (div)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup ((!))

view :: State -> HTML Event
view (State st) =
  div ! className "app" $ do
    style css

    case st.route of
      (Home) -> Homepage.view (State st)
      (NotFound url) -> NotFound.view (State st)

css :: CSS
css = do
  let green = rgb 14 196 172
      fillColor = fromMaybe (rgb 0 0 0) (fromHexString "#a9bdbd")
      lineColor = rgba 0 0 0 (0.15::Number)

  fromString ".col-md-6 > .cell" ? do
    minHeight  (300.0 #px)
    backgroundColor fillColor
    border solid (1.0 #px) lineColor
    borderRadius (10.0 #px) (10.0 #px) (10.0 #px) (10.0 #px)
    margin (6.0 #px) (6.0 #px) (6.0 #px) (6.0 #px)


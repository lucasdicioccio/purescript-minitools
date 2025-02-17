module Minitools.Bricks.ColorDot (dot, Shape(..), CssColor, CssSize, Props) where

import Prelude ((<>), show)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type CssColor = String

type CssSize = Int

data Shape
  = Circle

type Props =
  { color :: CssColor
  , size :: CssSize
  , shape :: Shape
  }

dot :: forall m s a. Props -> H.ComponentHTML a s m
dot props = HH.span [ HP.class_ (HH.ClassName "color-dot") , HP.style (dotStyle props) ] []

dotStyle :: Props -> String
dotStyle {color,size,shape} = case shape of
  Circle -> "height:" <> show size <> "px;width:" <> show size <> "px;display:inline-block;border-radius:50%;background-color:" <> color <> ";"


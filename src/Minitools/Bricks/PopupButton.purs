module Minitools.Bricks.PopupButton
  ( Props
  , render
  ) where

import Data.Maybe (Maybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type Props a s m =
  { title :: String
  , extraClass :: String
  , icon :: H.ComponentHTML a s m
  , onClick :: Maybe a
  }

render :: forall a s m. Props a s m -> H.ComponentHTML a s m
render props =
  HH.button
  [ HP.classes [ HH.ClassName "button" , HH.ClassName props.extraClass ]
  ]
  [ HH.span_
    [ HH.text props.title
    ]
  , HH.span
    [ HP.classes [ HH.ClassName "icon", HH.ClassName "is-small" ]
    ]
    [ props.icon
    ]
  ]

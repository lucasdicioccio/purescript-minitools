-- A brick for making HTML pop-up buttons.
--
-- The main use for this module is to provide buttons for PopupMenu but you can use it standalone.
-- See https://bulma.io/documentation/components/dropdown/ for the button
--
-- the Props type is parameterized by action slot and effect monad
-- the Props type contains a `title` string, an `extraClass` string (for instance to add `is-small`), an `icon` sub-component, and an `onClick` optional action
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

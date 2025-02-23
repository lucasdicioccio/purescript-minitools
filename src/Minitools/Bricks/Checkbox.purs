-- A brick for making HTML checkboxes.
--
-- the Props type is parameterized by action slot and effect monad
-- the Props type contains a `contents` (whole HTML sub component) which is rendered aside the checkbox, the `onChecked` action to map the boolean checked-status of the checkbox
module Minitools.Bricks.Checkbox
  ( Props
  , render
  ) where

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

type Props a s m =
  { checked :: Boolean
  , contents :: (H.ComponentHTML a s m)
  , onChecked :: Boolean -> a
  }

render :: forall m s a. Props a s m -> H.ComponentHTML a s m
render props =
  HH.label
  [ HP.classes [ HH.ClassName "checkbox" ]
  ]
  [ HH.input
    [ HP.type_ HP.InputCheckbox
    , HP.checked props.checked
    , HE.onChecked props.onChecked
    ]
  , props.contents
  ]

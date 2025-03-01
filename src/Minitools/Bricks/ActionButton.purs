-- A brick for making HTML Buttons
--
-- the Props type is parameterized by action
-- the Props type contains a `text` (show on the button), the `action` to trigger on clicking, a `disabled` boolean to prevent or allow clicks, and an `info` tooltip
-- an additional mode Props2 exists which also get a `classes` array of ClassName
module Minitools.Bricks.ActionButton (
    Props
  , render
  , Props2
  , render2
  ) where

import Prelude (const)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Props a =
  { text :: String
  , action :: a
  , disabled :: Boolean
  , info :: String
  }

render :: forall a s m. Props a -> HH.ComponentHTML a s m
render props =
  HH.button
  [ HP.classes [ HH.ClassName "button" ]
  , HP.title props.info
  , HE.onClick (const props.action)
  , HP.disabled props.disabled
  ]
  [ HH.text props.text
  ]

type Props2 a =
  { text :: String
  , action :: a
  , disabled :: Boolean
  , info :: String
  , classes :: Array HH.ClassName
  }

render2 :: forall a s m. Props2 a -> HH.ComponentHTML a s m
render2 props =
  HH.button
  [ HP.classes props.classes
  , HP.title props.info
  , HE.onClick (const props.action)
  , HP.disabled props.disabled
  ]
  [ HH.text props.text
  ]

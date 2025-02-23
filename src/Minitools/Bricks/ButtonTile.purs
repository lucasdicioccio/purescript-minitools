-- A brick for making 128x128 large square HTML Buttons
--
-- the Props type is parameterized by action slot and effect monad
-- the Props type contains a `contents` (whole HTML sub component), the `action` to trigger on clicking, a `disabled` boolean to prevent or allow clicks, and an `info` tooltip
module Minitools.Bricks.ButtonTile
  ( Props
  , render
  ) where

import Prelude (const)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Props a s m =
  { contents :: HH.ComponentHTML a s m
  , action :: a
  , disabled :: Boolean
  , info :: String
  }

render :: forall a s m. Props a s m -> HH.ComponentHTML a s m
render props =
  HH.a
  [ HP.classes [ HH.ClassName "button", HH.ClassName "image", HH.ClassName "is-128x128" ]
  , HE.onClick (const props.action)
  , HP.title props.info
  ]
  [ props.contents
  ]

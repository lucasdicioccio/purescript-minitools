-- A brick for making HTML Selects
--
-- the Props type is parameterized by action and by the contained `item`
-- the Props type contains a `text` (shown on the selector), an `items` list, a `disabled` boolean to allow/disallow modifications, and a pair of function `option` mapping an item to a rendered text, and an `action` to map the selected item into an action
module Minitools.Bricks.ActionSelector
  ( Props
  , render
  ) where

import Prelude (($),map)
import Data.Array as Array
import Data.List as List
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Props item a =
  { text :: String
  , items :: List.List item
  , option :: item -> String
  , action :: item -> a
  , disabled :: Boolean
  }

render :: forall item a s m. Props item a -> HH.ComponentHTML a s m
render props =
    HH.div
    [ HP.classes [ HH.ClassName "select" ]
    ]
    [ HH.select_
      options
    ]
  where
    options = Array.cons titleOption itemOptions
    titleOption = HH.option_ [ HH.text props.text ]
    itemOptions = List.toUnfoldable (map itemOption props.items)
    itemOption item =
      HH.option
      [ HE.onClick (\_ -> props.action item)
      ]
      [ HH.text $ props.option item
      ]

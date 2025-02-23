-- A brick for making HTML dropdowns menus.
--
-- See https://bulma.io/documentation/components/dropdown/ for bulma dropdowns.
--
-- the Props type is parameterized by action, slot, and effect monad
-- the Props type contains a string `title` for the menu, a `menuClass` applied to the drop-down button when collapsed, and a `menuIcon` arbitrary html, then an array of arrays of HTML subcomponents (the two levels or items organize items in sections)
--
-- Note that the module leverages a PopupButton brick.
module Minitools.Bricks.MenuPopup
  ( Props
  , render
  , divider
  ) where

import Prelude (($), map)
import Data.Maybe (Maybe(..))
import Data.Array as Array
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Minitools.Bricks.PopupButton as PopupButton

type Props a s m =
  { title :: String
  , menuClass :: String
  , menuIcon :: H.ComponentHTML a s m
  , items :: Array (Array (H.ComponentHTML a s m))
  }

render :: forall a s m. Props a s m -> H.ComponentHTML a s m
render props =
  HH.div
  [ HP.classes [ HH.ClassName "dropdown" , HH.ClassName "is-hoverable" ]
  ]
  [ HH.div
    [ HP.classes [ HH.ClassName "dropdown-trigger" ]
    ]
    [ PopupButton.render
      { title: props.title
      , onClick: Nothing
      , extraClass: props.menuClass
      , icon: props.menuIcon
      }
    ]
  , HH.div
    [ HP.classes [ HH.ClassName "dropdown-menu" ]
    ]
    [ HH.div
      [ HP.classes [ HH.ClassName "dropdown-content" ]
      ]
      $ Array.intercalate [divider]
      $ map renderItems props.items
    ]
  ]

renderItems :: forall a s m. Array (H.ComponentHTML a s m) -> Array (H.ComponentHTML a s m)
renderItems items = map renderItem items

renderItem :: forall a s m. H.ComponentHTML a s m -> H.ComponentHTML a s m
renderItem item =
  HH.div
  [ HP.classes [ HH.ClassName "dropdown-item" ]
  ]
  [ item
  ]

divider :: forall a s m. H.ComponentHTML a s m
divider = HH.hr [ HP.classes [ HH.ClassName "dropdown-divider" ] ]

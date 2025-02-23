-- A brick for making filterable menus inside popups.
--
-- The brick is a specialized MenuPopup with a search input as first item.
--
-- the Props type is parameterized by item type, action, slot and effect monad
-- the Props type contains a string `title` for the menu, an `onSearch` handler taking the searched string as input, an array `foundItems` for the items to display, and a `renderItem` function to turn an item into an array of sub-components
module Minitools.Bricks.SearchPopup
  ( Props
  , render
  ) where

import Prelude
import Data.Array as Array
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Minitools.Bricks.MenuPopup as MenuPopup

type Props item a s m =
  { title :: String
  , onSearch :: String -> a
  , foundItems :: Array item
  , renderItem :: item -> Array (H.ComponentHTML a s m)
  , menuClass :: String
  , menuIcon :: H.ComponentHTML a s m
  }

render :: forall item a s m. Props item a s m -> H.ComponentHTML a s m
render props@{title,menuClass,menuIcon} =
  MenuPopup.render
  { title
  , items
  , menuClass
  , menuIcon
  }
  where
    items = [[searchItem], foundItems]

    searchItem =
      HH.p
      [ HP.class_ $ HH.ClassName "control"
      ]
      [ HH.input
        [ HP.class_ $ HH.ClassName "input"
        , HP.placeholder "search"
        , HE.onValueInput props.onSearch
        ]
      ]

    foundItems =
      Array.concatMap props.renderItem props.foundItems

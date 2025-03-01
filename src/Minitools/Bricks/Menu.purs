-- A brick for making HTML menus as styled in Bulma
-- See https://bulma.io/documentation/components/menu/ for the menu.
--
-- the Props type is parameterized by action, slot, effect monad and type of item
-- the module defines utility types MenuRow and Item
-- - MenuRow is a sumtype do distinguish between menu labels (unclickable separators) and proper item arrays
-- - an Item contains a rendered `contents`, the `datum` object itself, an `onClick` handler, and `subItems` as a children array
-- the Props type then contains an array of MenuRow named `items`, and an `isActive` predicate function which tells if a given MenuRow is active or not based on the datum
--
-- Note that even though the type allows for has many nesting levels one can
-- imagine, the Bulma component contains at most two levels.
module Minitools.Bricks.Menu
  ( Props
  , MenuRow(..)
  , Item(..)
  , render
  ) where

import Prelude (($),map)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.UIEvent.MouseEvent (MouseEvent)

data MenuRow datum a s m
  = Label String
  | Items (Array (Item datum a s m))

data Item datum a s m =
    Item
    { contents :: HH.ComponentHTML a s m
    , datum :: datum
    , onClick :: MouseEvent -> datum -> a
    , subItems :: Array (Item datum a s m)
    }

-- todo: pass items extra classes
type Props datum a s m =
  { items :: Array (MenuRow datum a s m)
  , isActive :: datum -> Boolean
  }

render :: forall datum a s m. Props datum a s m -> HH.ComponentHTML a s m
render props =
  HH.aside
  [ HP.classes [ HH.ClassName "menu" ]
  ]
  $ map renderItem props.items
  where
    renderItem (Label name) =
      HH.p
      [ HP.classes [ HH.ClassName "menu-label" ]
      ]
      [ HH.text name
      ]
    renderItem (Items items) =
      HH.ul
      [ HP.classes [ HH.ClassName "menu-list" ]
      ]
      $ map renderListItem items

    renderListItem (Item item) =
      HH.li_
      [ HH.a
        [ HP.class_ $ if props.isActive item.datum then HH.ClassName "is-active" else HH.ClassName ""
        , HE.onClick (\ev -> item.onClick ev item.datum)
        ]
        [ item.contents
        ]
      , HH.ul_
        $ map renderListItem item.subItems
      ]


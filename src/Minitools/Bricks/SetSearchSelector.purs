-- A brick for making a selection in a popup and out of many items.
-- A text input allows to filter the display among items.
--
-- The brick is a specialized SearchPopup with Checkbox-decorated items.
--
-- the Props type is parameterized by item type, action, slot and effect monad
-- the Props type contains a `title` string for the menu
--   - a list of all selectable `items`
--   - a `searchString` which contains the state of the current text filter
--   - a `matchItem` predicate based on the searchString and individual items controls whether to show an item or not.
--   - a `renderItem` function allows to tune the sub-component
--   - an `activeItem` predicate tells whether an item is selected or not
--   - an `onSearch` handler turns changes of text filter into an action
--   - an `onChecked` handler turns the wanted selected/unselected state of an item into an action
--   - an `renderNotFound` sub-component allow to display a placeholder when nothing is found
--   - a `title` string, a `menuClass` string and a `menuIcon` sub-component are all passed to SearchPopup
module Minitools.Bricks.SetSearchSelector
  ( Props
  , render
  )where

import Data.List as List
import Data.List (List)
import Halogen as H

import Minitools.Bricks.Checkbox as Checkbox
import Minitools.Bricks.SearchPopup as SearchPopup

type Props item a s m =
  { title :: String
  , items :: List item
  , searchString :: String
  , matchItem :: String -> item -> Boolean
  , renderItem :: item -> H.ComponentHTML a s m
  , activeItem :: item -> Boolean
  , onSearch :: String -> a
  , onChecked :: item -> Boolean -> a
  , menuClass :: String
  , menuIcon :: H.ComponentHTML a s m
  , renderNotFound :: H.ComponentHTML a s m
  }

data NotFound = NotFound

render :: forall item a s m. Props item a s m -> H.ComponentHTML a s m
render props@{title,onSearch,menuClass,menuIcon} =
   if List.null resultFound
   then
     SearchPopup.render
     { title
     , onSearch
     , foundItems: [NotFound]
     , renderItem: \NotFound -> [ props.renderNotFound ]
     , menuClass
     , menuIcon
     }
   else
     SearchPopup.render
     { title
     , onSearch
     , foundItems: 
         List.toUnfoldable resultFound
     , renderItem: \item -> [ fieldDisplayedItem item ]
     , menuClass
     , menuIcon
     }
  where
    resultFound = List.filter (props.matchItem props.searchString) props.items
    fieldDisplayedItem item =
      Checkbox.render
      { checked: props.activeItem item
      , contents: props.renderItem item
      , onChecked: props.onChecked item
      }


-- | SearchPopup where the items are clickable (checkboxes) selections.
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

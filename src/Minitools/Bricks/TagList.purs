
module Minitools.Bricks.TagList
  ( Props
  , render
  ) where

import Prelude (($),const,map)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Props item a s m =
  { items :: Array item
  , contents :: item -> H.ComponentHTML a s m
  , onDelete :: item -> Maybe a
  }

render :: forall item a s m. Props item a s m -> H.ComponentHTML a s m
render props =
  HH.div
  [ HP.classes [ HH.ClassName "tags" ]
  ]
  $ map (render_item props) props.items

render_item :: forall item a s m. Props item a s m -> item -> H.ComponentHTML a s m
render_item props item =
  HH.span
  [ HP.classes [ HH.ClassName "tag" ]
  ]
  [ props.contents item
  , case props.onDelete item of
    Nothing -> HH.text ""
    Just action ->
      HH.button
      [ HP.classes [ HH.ClassName "delete" , HH.ClassName "is-small" ]
      , HE.onClick (const action)
      ]
      [
      ]
  ]

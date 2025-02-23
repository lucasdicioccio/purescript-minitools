-- A brick for making a row of KPI items with a title and a value. Each item is clickable.
--
-- the Props type is parameterized by action and type of item
-- the Props type contains an array of `kpis` items
-- each item contains a `name` and a `value` strings, the `item` itself and an `onClick` handler
module Minitools.Bricks.KPIs
  ( Props
  , Item
  , render
  ) where

import Prelude (($),map)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.UIEvent.MouseEvent (MouseEvent)

type Item datum a =
  { name :: String
  , value :: String
  , datum :: datum
  , onClick :: MouseEvent -> datum -> a
  }

type Props datum a =
  { kpis :: Array (Item datum a)
  }

render :: forall datum a s m. Props datum a -> H.ComponentHTML a s m
render props = 
  HH.nav
  [ HP.class_ $ HH.ClassName "level"
  ]
  $ map renderItem props.kpis
  where
    renderItem item =
       HH.div
       [ HP.classes [ HH.ClassName "level-item", HH.ClassName "has-text-centered" ]
       ]
       [ HH.p
         [ HE.onClick (\ev -> item.onClick ev item.datum)
         ]
         [ HH.p
           [ HP.class_ $ HH.ClassName "heading"
           ]
           [ HH.text item.name
           ]
         , HH.a
           [ HP.class_ $ HH.ClassName "title"
           ]
           [ HH.text item.value
           ]
         ]
       ]


module Minitools.Bricks.Tabs
  ( Props
  , Tab
  , render
  ) where

import Prelude (($),map)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.UIEvent.MouseEvent (MouseEvent)

type Tab datum a s m =
  { contents :: HH.ComponentHTML a s m
  , datum :: datum
  , onClick :: MouseEvent -> datum -> a
  }

-- todo: pass tabs extra classes
type Props datum a s m =
  { tabs :: Array (Tab datum a s m)
  , isActive :: Tab datum a s m -> Boolean
  }

render :: forall datum a s m. Props datum a s m -> HH.ComponentHTML a s m
render props =
  HH.div
  [ HP.classes [ HH.ClassName "tabs" ]
  ]
  [ HH.ul_
    $ map renderTab props.tabs
  ]
  where
    renderTab tab =
      HH.li
      [ HP.class_ $ if props.isActive tab then HH.ClassName "is-active" else HH.ClassName ""
      , HE.onClick (\ev -> tab.onClick ev tab.datum)
      ]
      [ HH.a_
        [ tab.contents
        ]
      ]

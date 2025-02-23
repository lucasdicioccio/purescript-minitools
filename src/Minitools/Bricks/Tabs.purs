-- A brick for making a HTML tabs as an ul/li/a nesting.
-- 
-- Leverages the tabs class from https://bulma.io/documentation/components/tabs/ bulma.
--
-- the Props type is parameterized by action, slot, effect monad and type of item
-- the Props type contains a `tabs` array of Tab item and an `isActive` predicate based on the tab to decide if a tab is active or not
-- each Tab item contains a `contents` sub-component representing the title of the tab, a `datum`, and an `onClick` event-handler mapping the datum into an action
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

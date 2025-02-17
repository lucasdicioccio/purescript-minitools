
module Minitools.Bricks.Details
  ( Props
  , render
  ) where

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type Props a s m =
  { summary :: String
  , details :: Array (H.ComponentHTML a s m)
  }

render :: forall m s a. Props a s m -> H.ComponentHTML a s m
render props =
  HH.details
  [ HP.classes [ HH.ClassName "details" ]
  ]
  [ HH.summary
    [ HP.class_ (HH.ClassName "details-title")
    ]
    [ HH.text props.summary
    ]
  , HH.div
    [ HP.class_ (HH.ClassName "details-contents")
    ]
    props.details
  ]


module Minitools.Bricks.Message
  ( Props
  , MessageLevel(..)
  , Details(..)
  , render
  ) where

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

data MessageLevel
  = Error
  | Notice
  | Success

data Details a s m
  = NoDetails
  | ExtraDetails String
  | Contents (H.ComponentHTML a s m)

type Props a s m =
  { level :: MessageLevel
  , main :: String
  , details :: Details a s m
  }

render :: forall a s m.  Props a s m -> H.ComponentHTML a s m
render props = 
    HH.div
    [ HP.classes [ HH.ClassName "message", HH.ClassName lvlClass ]
    ]
    children
  where
    header txt =
      HH.div
      [ HP.classes [ HH.ClassName "message-header" ]
      ]
      [ HH.p_ [ HH.text txt ]
      ]
    body sub =
      HH.div
      [ HP.classes [ HH.ClassName "message-body" ]
      ]
      [ sub
      ]
    children = case props.details of
      NoDetails -> [body (HH.p_ [ HH.text props.main ])]
      ExtraDetails dtls -> [header props.main, body (HH.p_ [ HH.text dtls ])]
      Contents dtls -> [header props.main, body dtls]

    lvlClass = case props.level of
      Notice -> "is-info"
      Success -> "is-success"
      Error -> "is-warning"

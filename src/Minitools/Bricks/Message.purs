-- A brick for making HTML message boxes.
--
-- See https://bulma.io/documentation/components/message/ for bulma messages.
--
-- the Props type is parameterized by action, slot and effect monad
-- the Props type contains a `level` for the message importance, a `main` string for the title of the message, and a `details` object
-- the level of the message is a sum-type for error, notice, and success
-- the details object, is a sum-type for when no-detail is given, a simple textual string is given, or when a proper sub-component is given
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

-- | An utility for remote-data.
module Minitools.Api.Base
  ( Remote(..)
  , isOngoing
  , mapRemote
  , mapResponse
  ) where

import Effect.Aff (Aff)
import Data.Either (Either)
import Affjax.Web as AX
import Data.Argonaut.Decode.Error (JsonDecodeError)
import Data.Functor (class Functor)

---------------------------------------------------------------------

data Remote a
  = Uninitialized
  | Loading
  | Errored String
  | Got a

-- | A predicate to tell whether we expect the value to change in the future.
isOngoing :: forall a. Remote a -> Boolean
isOngoing Uninitialized = false
isOngoing (Errored _) = false
isOngoing Loading = true
isOngoing (Got _) = false

mapRemote :: forall a b. (a -> b) -> Remote a -> Remote b
mapRemote _ Uninitialized = Uninitialized
mapRemote _ Loading = Loading
mapRemote _ (Errored err) = (Errored err)
mapRemote f (Got a) = Got (f a)

instance Functor Remote where
  map = mapRemote

---------------------------------------------------------------------

type BaseUrl =
  { path :: String -> String
  }

---------------------------------------------------------------------

mapResponse :: forall a b. (a -> b) -> AX.Response a -> AX.Response b
mapResponse f r = r { body = f r.body }

type AffJ a =
  Aff (Either AX.Error (AX.Response (Either JsonDecodeError a)))

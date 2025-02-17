
module Minitools.Tracer where

import Prelude (($),(<<<), discard, Unit)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

type Tracer a = a -> Effect Unit

trace :: forall a m. MonadEffect m => Tracer a -> a -> m Unit
trace f a = liftEffect $ f a

comap :: forall a b. (b -> a) -> Tracer a -> Tracer b
comap f g = g <<< f

traceBoth :: forall a. Tracer a -> Tracer a -> Tracer a
traceBoth f1 f2 = \item -> do
  f1 item
  f2 item

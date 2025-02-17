module Minitools.BackgroundLoop (
  timer,
  timerWithErrorHandler
  ) where

import Prelude (($),Unit,bind,discard,pure,show)
import Halogen.Subscription (makeEmitter, Emitter)
import Effect (Effect)
import Effect.Console (log)
import Effect.Class (liftEffect)
import Effect.Exception (Error, error)
import Control.Monad.Rec.Class (forever)
import Data.Either (Either(..))
import Effect.Aff (Milliseconds)
import Effect.Aff as Aff

-- | A Timer that shows error in the console.
timer :: forall action. Milliseconds -> action -> Emitter action
timer ms act = 
    timerWithErrorHandler onError ms act
  where
    onError :: Either Error Unit -> Effect Unit
    onError (Left err) = log (show {err,situation:"onError"})
    onError (Right _)  = log "done"

timerWithErrorHandler
  :: forall action.
  (Either Error Unit -> Effect Unit) ->
  Milliseconds ->
  action ->
  Emitter action
timerWithErrorHandler onError ms act = 
  makeEmitter \emitter -> do
    fiber <- Aff.runAff onError $ do
      forever $ do
        liftEffect (emitter act)
        Aff.delay ms
    let stop = do
           Aff.launchAff_ (Aff.killFiber (error "stopping timer fiber") fiber)
    pure stop

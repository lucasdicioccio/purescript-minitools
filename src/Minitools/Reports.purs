
-- todo:
-- * coalesece
module Minitools.Reports
 ( init
 , Recording
 , RefreshString
 , generateRecordingRefresh
 , Pusher
 , Push
 , AnonymousPush
 , newPusher
 , pushItem
 )
 where

---------------------------------------------------------------------------
import Prelude

import Affjax as AX
import Affjax.Web as AXWeb
import Affjax.ResponseFormat as ResponseFormat
import Affjax.RequestBody as RequestBody
import Control.Monad.Loops (unfoldM)
import Control.Monad.Rec.Class (forever)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (encodeJson)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Int as Int
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.String.Unsafe (charAt)
import Data.String.CodePoints (length)
import Data.String.CodeUnits (fromCharArray)
import Data.Time.Duration (Milliseconds(..))
import Data.Unfoldable (replicateA)
import Effect (Effect)
import Effect.Aff (Aff, delay, launchAff_)
import Effect.AVar as AVar
import Effect.Aff.AVar as Affvar
import Effect.Random (randomInt)
---------------------------------------------------------------------------

type RefreshString = String

generateRecordingRefresh :: Effect RefreshString
generateRecordingRefresh = randomb64s 16

randomb64s :: Int -> Effect String
randomb64s c = do
    map fromCharArray $ replicateA c $ map pickChar (randomInt 0 255)
  where
    pickChar n = charAt (mod n (length randchar)) randchar
    randchar = "1234567890qwertyuiopasdfghjklmzxcvbnQWERTYUIOPASDFGHJKLMZXCVBN=-"

-- corresponds to a session/configuration
type Recording =
  { baseUrl :: String
  , refreshString :: RefreshString
  }

-- corresponds to a single event to push
type Push =
  { e :: Json
  , r :: RefreshString
  }

type AnonymousPush =
  { e :: Json
  }

-- a system to record, buffer, and send-out push
type Pusher = AVar.AVar Push

newPusher :: Effect Pusher
newPusher = AVar.empty

pushItem :: Pusher -> Push -> Effect Unit
pushItem pusher push = void $ AVar.put push pusher callback
  where
    callback _ = pure unit

-- Drain all items from a pusher.
drain :: Pusher -> Aff (Array Push)
drain pusher =
    unfoldM waitTake
  where
    waitTake = do
      delay (Milliseconds 200.0)
      Affvar.tryTake pusher

-- Send pushes for a given recording.
publishPushes :: Recording -> Array Push -> Aff (Maybe (Either AX.Error (AX.Response Json)))
publishPushes _ []  = pure Nothing
publishPushes recording pushes = Just <$> do
  AX.post
    AXWeb.driver
    ResponseFormat.json
    recording.baseUrl
    (Just $ RequestBody.json $ encodeJson pushes)

periodicallyPublishPush :: Recording -> Pusher -> Effect Unit
periodicallyPublishPush recording pusher = do
  launchAff_ $ forever $ do
    delay (Milliseconds 3000.0)
    items <- drain pusher
    let chunks = chunkItems items
    for_ chunks $ \chunk -> do
      goChunk 0 chunk
  where
    chunkItems :: Array Push -> Array (Array Push)
    chunkItems items = fChunk [] items
    fChunk :: forall a. Array (Array a) -> Array a -> Array (Array a)
    fChunk chunks [] = chunks
    fChunk chunks xs =
      let { after, before } = Array.splitAt 100 xs
      in if Array.null before
         then chunks
         else fChunk (Array.cons before chunks) after

    backoffNoEvents :: Aff Unit
    backoffNoEvents = delay (Milliseconds 7000.0)

    continue :: Aff Unit
    continue = pure unit

    backoffError :: Int -> Aff Unit
    backoffError n = delay (Milliseconds $ Int.toNumber $ max 10000 (n * 1000))
   
    goChunk :: Int -> Array Push -> Aff Unit
    goChunk n chunk 
      | n > 10 = pure unit
      | otherwise = do
          res <- publishPushes recording chunk
          case res of
            Nothing ->  backoffNoEvents
            (Just (Right _)) -> continue
            (Just (Left _)) -> do
              backoffError n
              goChunk (n+1) chunk

init :: Recording -> Effect (AnonymousPush -> Effect Unit)
init recording = do
  pusher <- newPusher
  periodicallyPublishPush recording pusher
  pure (\{e} -> pushItem pusher {e, r: recording.refreshString})

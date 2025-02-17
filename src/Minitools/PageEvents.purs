
module Minitools.PageEvents (Config,defaultConfig,tapElement,Annotation,Attributes) where

import Prelude (($),(#),(=<<),(>>=),Unit,bind,map,pure)
import Data.Foldable (for_)
import Data.Array ((:))
import Data.Array as Array
import Data.Maybe (Maybe(..),maybe)
import Data.String (split, Pattern(..))
import Data.Tuple (Tuple(..))
import Web.Event.Event (Event, EventType(..), type_, target)
import Web.Event.EventTarget (EventTarget, addEventListener, eventListener)
import Web.DOM.Node (parentNode, fromEventTarget)
import Web.DOM.Element (Element, getAttribute, toNode, fromNode, id, tagName)
import Effect (Effect)

---------------------------------------------------------------------------

import Minitools.Tracer (Tracer)

---------------------------------------------------------------------------

type Config =
  { recordedEvents :: Array String
  , annotationsKey :: String
  , annotationsKeyValues :: String
  }

defaultConfig :: Config
defaultConfig =
  { recordedEvents: [ "click", "select", "data", "change" ]
  , annotationsKey: "data-ev"
  , annotationsKeyValues: "data-kv"
  }

type Annotation = 
  { elementID :: String
  , eventType :: String
  , attributes :: Array Attributes
  }

tapElement :: Tracer Annotation -> Config -> EventTarget -> Effect Unit
tapElement tracer config eventTarget = do
  for_ config.recordedEvents $ \ev ->
    startLogging eventTarget (logEvent tracer config) ev

startLogging :: EventTarget -> (Event -> Effect Unit) -> String -> Effect Unit
startLogging tgt handler name = do
  listener <- eventListener handler
  addEventListener (EventType name) listener true tgt

logEvent :: Tracer Annotation -> Config -> Event -> Effect Unit
logEvent tracer config ev = do
  let mElt = fromNode =<< fromEventTarget =<< target ev
  for_ mElt $ \elt ->
    logEventOnElement tracer config ev elt

logEventOnElement :: Tracer Annotation -> Config -> Event -> Element -> Effect Unit
logEventOnElement tracer config ev elt = do
  let (EventType eventType) = type_ ev
  _id <- id elt
  let _tag = tagName elt
  attributes <- findAnnotations config elt
  tracer { elementID: _id, eventType, attributes }

findAnnotations :: Config -> Element -> Effect (Array Attributes)
findAnnotations = findAnnotations' []

type Attributes =
  { k :: String
  , kvs :: Array (Tuple String String)
  }

findAnnotations' :: Array Attributes -> Config -> Element -> Effect (Array Attributes)
findAnnotations' prev config elt = do
  nodeContent <- getAttribute config.annotationsKeyValues elt
  let kvs = maybe [] extractKeyValues nodeContent

  nodeKey <- getAttribute config.annotationsKey elt
  let items = maybe prev (\k -> {k, kvs} : prev) nodeKey

  mbParent <- parentNode (toNode elt)
  case mbParent >>= fromNode of
    Nothing -> pure items
    Just parent -> findAnnotations' items config parent

extractKeyValues :: String -> Array (Tuple String String)
extractKeyValues str =
    split (Pattern ";") str
      # map (\kAndV -> split (Pattern "=") kAndV)
      # map toTuple
      # Array.catMaybes
  where
    toTuple items =
      case Array.take 2 items of
        [x,y] -> Just (Tuple x y)
        _ -> Nothing

module Main where

import Prelude

import Autocomplete.Component as Autocomplete
import Autocomplete.Events as Events
import Control.Coroutine as CR
import Data.Foldable (for_)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Class.Console (log)
import Halogen (liftEffect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.DOM.Element (Element, getAttribute, toEventTarget, toNode)
import Web.DOM.Node (Node, childNodes, removeChild)
import Web.DOM.NodeList (toArray)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.Event.CustomEvent (toEvent)
import Web.Event.EventTarget (dispatchEvent)
import Web.HTML.HTMLElement (toElement)

main :: Effect Unit
main = HA.runHalogenAff do
  _ <- HA.awaitLoad
  container <- HA.selectElement (QuerySelector "#geocode-city-autocomplete")
  case container of
    Nothing -> liftEffect $ log "Please provide a container with id `geocode-city-autocomplete`"
    Just el -> do
      -- clear out all children of the container.
      let asElement = toElement el
      _ <- liftEffect $ removeChildren (toNode asElement)
      cfg <- liftEffect $ extractConfig asElement
      io <- runUI Autocomplete.component cfg el
      let evtTarget = toEventTarget $ asElement
      io.subscribe $ CR.consumer \msg ->
        case msg of
          (Autocomplete.ResultsFound cities) -> do
            _ <- liftEffect $ dispatchEvent (toEvent $ Events.citiesFound cities) evtTarget
            pure Nothing
          Autocomplete.NoResults -> do
            _ <- liftEffect $ dispatchEvent (toEvent $ Events.noCitiesFound) evtTarget
            pure Nothing
          (Autocomplete.Selected city) -> do
            _ <- liftEffect $ dispatchEvent (toEvent $ Events.citySelected city) evtTarget
            --liftEffect $ log $ "Selected: " <> city.name
            pure Nothing
          (Autocomplete.Failed errMsg) -> do
            _  <- liftEffect $ dispatchEvent (toEvent $ Events.lookupError errMsg) evtTarget
            --liftEffect $ log $ "failed: " <> errMsg
            pure Nothing


removeChildren :: Node -> Effect Unit
removeChildren parent = do
  children <- childNodes parent >>= toArray
  for_ children (\child -> removeChild child parent)

extractConfig :: Element -> Effect Autocomplete.Config
extractConfig element = do
  apiKey <- getAttribute "data-api-key" element
  inputName <- getAttribute "data-input-name" element
  inputClass <- getAttribute "data-input-class" element
  -- the API defaults to 5
  suggestionCountStr <-  getAttribute "data-suggestion-count" element
  let suggestionCount = fromMaybe Nothing $ fromString <$> suggestionCountStr
  pure $ {apiKey, inputName, inputClass, suggestionCount}

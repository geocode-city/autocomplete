module Autocomplete.Events where

import Autocomplete.Internal (City)
import Data.Argonaut (Json, encodeJson)
import Data.Function.Uncurried (Fn2, runFn2)
import Web.Event.CustomEvent (CustomEvent)

foreign import customEvent :: Fn2 String Json CustomEvent

customEventC :: String -> Json -> CustomEvent
customEventC = runFn2 customEvent

citySelected :: City -> CustomEvent
citySelected city = customEventC "citySelected" (encodeJson city)

lookupError :: String -> CustomEvent
lookupError errMsg = customEventC "lookupError" (encodeJson {message: errMsg})

citiesFound :: Array City -> CustomEvent
citiesFound cities = customEventC "citiesFound" (encodeJson cities)

noCitiesFound :: CustomEvent
noCitiesFound = customEventC "noCitiesFound" (encodeJson {message: "No results."})

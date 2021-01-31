module Autocomplete.Internal where

import Prelude

import Affjax (Error(..))
import Affjax as AX
import Affjax.ResponseFormat as AR
import Affjax.StatusCode (StatusCode(..))
import Data.Argonaut (decodeJson)
import Data.Either (Either(..))
import Data.Int (decimal, toStringAs)
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.String (null)
import Effect.Aff (Aff)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Network.RemoteData as RD


type City = {
  name :: String,
  longitude :: Number,
  latitude :: Number,
  country :: Maybe String,
  countryCode :: Maybe String,
  region :: Maybe String,
  district :: Maybe String,
  timezone :: String,
  population :: Int
}

-- | Makes an autocomplete request to the service, as specified in:
-- | https://geocode.city/docs.html
-- 
-- References:
-- https://pursuit.purescript.org/packages/purescript-affjax/11.0.0
-- https://pursuit.purescript.org/packages/purescript-argonaut-codecs/7.0.0
-- https://github.com/citizennet/purescript-halogen-select/blob/v5.0.0/examples/Components/Typeahead.purs
autocompleteCandidates :: String -> Maybe String -> Maybe Int -> Aff (RD.RemoteData String (Array City))
autocompleteCandidates partialCity apiKey limit' = do
  res <- AX.get AR.json ("https://api.geocode.city/autocomplete" <> prefix <> "q=" <> partialCity <> "&limit=" <> (toStringAs decimal limit))
  case res of
    Left err -> 
      case err of 
        -- unfortunately, our backend doesn't return JSON for failures,
        -- so we resort to switching on status codes.
        ResponseBodyError _ resp ->
          case resp.status of
            StatusCode 401 -> failed "Missing API Key."
            StatusCode 403 -> failed "Invalid API Key."
            StatusCode 429 -> failed "Request limit reached."
            _ -> failed $ AX.printError err
        _ -> failed $ AX.printError err
    Right response -> pure $ RD.fromEither $ decodeJson response.body
  where
    failed = pure <<< RD.Failure
    prefix = maybe "?" (\k -> "?api-key=" <> k <> "&") apiKey
    limit  = fromMaybe 5 limit'


-- | Given a `City`, return a suitable one-line representation
cityString :: City -> String
cityString city =
  city.name <> separator <> location
  where
    location  = cityLocation city
    separator = if null location then "" else ", "

-- | Return the "rest" of a city's context: district, region, country
cityLocation :: City -> String
cityLocation city =
  district <> region <> country
  where
    district = maybe "" (_ <> ", ") city.district
    region   = maybe "" (_ <> ", ") city.region
    country  = maybe "" identity    city.country


--- CSS helpers
-- from:
-- https://github.com/citizennet/purescript-halogen-select/blob/256d09dfedf3aba3e6ed004753f0d89cea7cc993/examples/Internal/CSS.purs
class_ :: forall p i. String -> HH.IProp (class :: String | i) p
class_ = HP.class_ <<< HH.ClassName

classes_ :: forall p i. Array String -> HH.IProp (class :: String | i) p
classes_ = HP.classes <<< map HH.ClassName

whenElem :: forall p i. Boolean -> (Unit -> HH.HTML i p) -> HH.HTML i p
whenElem cond render = if cond then render unit else HH.text ""

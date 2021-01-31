module Autocomplete.Component where

import Autocomplete.Dropdown as D
import Autocomplete.Internal (City, autocompleteCandidates, cityLocation, cityString, class_, classes_, whenElem)
import Data.Array (length, mapWithIndex, null, (!!))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Monoid (guard, (<>))
import Data.String as String
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Network.RemoteData as RD
import Prelude (Unit, bind, discard, not, otherwise, pure, unit, (#), ($), (<<<), (==))
import Select as S
import Select.Setters as SS

type State = (
  selection :: Maybe City
, available :: RD.RemoteData String (Array City)
, config :: Config
)

data Action = HandleDropdown D.Message

data Message 
  = ResultsFound (Array City)
  | NoResults
  | Selected City
  | Failed String

type Config = {
  apiKey :: Maybe String
, inputName :: Maybe String
, inputClass :: Maybe String
, suggestionCount :: Maybe Int
}

data Query a = GetSelected (Maybe City -> a)

type Slot = S.Slot Query ChildSlots Message
type ChildSlots = (dropdown :: D.Slot Unit)

component :: H.Component HH.HTML (S.Query Query ChildSlots) Config Message Aff
component = S.component input $ S.defaultSpec
  {
    render = render
  , handleAction = handleAction
  , handleQuery = handleQuery
  , handleEvent = handleEvent
  }
  where
  input :: Config -> S.Input State
  input config = 
    {
      inputType: S.Text
    , debounceTime : Just (Milliseconds 300.0)
    , search: Nothing
    , getItemCount: maybe 0 length <<< RD.toMaybe <<< _.available
    , selection: Nothing
    , available: RD.NotAsked
    , config: config
    }

  handleEvent :: S.Event -> H.HalogenM (S.State State) (S.Action Action) ChildSlots Message Aff Unit
  handleEvent = case _ of
    S.Selected ix -> do
      st <- H.get
      for_ st.available \arr ->
        for_ (arr !! ix) \item -> do
          let newSelection = item
          H.modify_ _
            { selection = Just item
            , search = ""
            , visibility = S.Off
            }
          H.raise $ Selected newSelection  
    S.Searched str -> do
      st <- H.get
      if String.null str then do
        H.modify_ _ { available = RD.NotAsked }
      else do
        H.modify_ _ { available = RD.Loading }
        cities <- H.liftAff $ autocompleteCandidates str st.config.apiKey st.config.suggestionCount
        H.modify_ _ { available = cities }
        case cities of
          RD.Failure errMsg -> H.raise $ Failed errMsg
          RD.Success results -> do
            if null results then
              H.raise $ NoResults
            else
              H.raise $ ResultsFound results
          _ -> pure unit
    S.VisibilityChanged _ -> pure unit

  handleQuery :: forall a. Query a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = case _ of
    GetSelected reply -> do
      st <- H.get
      pure $ Just $ reply st.selection

  handleAction :: Action -> H.HalogenM (S.State State) (S.Action Action) ChildSlots Message Aff Unit
  handleAction = case _ of
    HandleDropdown msg -> case msg of
      D.SelectionChanged _oldCity newCity -> do
        st <- H.get
        H.modify_ _ {selection = newCity}

  render :: S.State State -> H.ComponentHTML (S.Action Action) ChildSlots Aff
  render st =
    HH.div
      [ class_ "geocode-city__autocomplete" ]
      [ renderInput, renderDropdown, renderContainer ]
    where
    hasSelection = isJust st.selection

    renderInput = HH.input $ SS.setInputProps
      [ classes_
          [ "geocode-city__autocomplete-input"
          , "geocode-city__autocomplete-input-selections" # guard hasSelection
          , "geocode-city__autocomplete-input-active" # guard (st.visibility == S.On)
          , fromMaybe "" st.config.inputClass # guard (isJust st.config.inputClass)
          ]
      , HP.name $ fromMaybe "" st.config.inputName
      , HP.placeholder "Type to search..."
      , HP.value $ maybe "" cityString st.selection
      ]

    renderPoweredBy =
      HH.div [class_ "geocode-city__powered-by"] [HH.small_ [
        HH.text "Powered by "
      , HH.a [HP.href "https://geocode.city"] [HH.text "geocode.city"]
      ]]

    renderDropdown = whenElem (st.visibility == S.On) \_ ->
      HH.slot _dropdown unit D.component dropdownInput handler
      where
      _dropdown = SProxy :: SProxy "dropdown"
      handler msg = Just $ S.Action $ HandleDropdown msg
      dropdownInput = { items: [] }

    renderContainer = whenElem (st.visibility == S.On) \_ ->
      HH.div
        (SS.setContainerProps
          [ classes_
              [ "geocode-city__autocomplete-container"
              , "geocode-city__autocomplete-container--" <> messageClass 
              ]
          ]
        )
        [HH.div_ renderItems, renderPoweredBy]
      where
      hasItems = maybe false (not <<< null) (RD.toMaybe st.available)

      messageClass =
        case st.available of
          RD.NotAsked -> "not-asked"
          RD.Loading -> "loading"
          RD.Failure _ -> "error"
          RD.Success avail
            | hasItems -> "results-found"
            | otherwise -> "no-results"

      renderItems = do
        let renderMsg msg = [ HH.span_ [ HH.text msg ] ]
        case st.available of
          RD.NotAsked -> renderMsg "No search performed..."
          RD.Loading -> renderMsg "Loading..."
          RD.Failure e -> renderMsg e
          RD.Success available
            | hasItems -> renderItem `mapWithIndex` available
            | otherwise -> renderMsg "No results found"

      renderItem index city =
        HH.div
          (SS.setItemProps index [ classes_ [ base, highlight, "geocode-city__city" ] ])
          [ HH.span
              [ class_ "geocode-city__city-name" ]
              [ HH.text $ city.name <> " "]
          , HH.span
              [ class_ "geocode-city__city-location" ]
              [ HH.text (cityLocation city) ]
          ]
        where
        base = "geocode-city__autocomplete-item"
        highlight = "geocode-city__autocomplete-item-highlighted" # guard (st.highlightedIndex == Just index)

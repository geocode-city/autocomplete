-- from:
-- https://github.com/citizennet/purescript-halogen-select/blob/256d09dfedf3aba3e6ed004753f0d89cea7cc993/examples/Components/Dropdown.purs
module Autocomplete.Dropdown where

import Autocomplete.Internal (City, cityString, class_, classes_, whenElem)
import Prelude (Unit, bind, discard, pure, unit, (#), ($), (<<<), (==))

import Data.Array ((!!), mapWithIndex, length)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Select as S
import Select.Setters as SS

type Slot =
  H.Slot S.Query' Message

type State =
  ( items :: Array City
  , selection :: Maybe City
  )

data Message
  = SelectionChanged (Maybe City) (Maybe City)

-- it is unnecessary to export your own input type, but doing so helps if you
-- would like to set some sensible defaults behind the scenes.
type Input =
  { items :: Array City
  }

component :: H.Component HH.HTML S.Query' Input Message Aff
component = S.component input $ S.defaultSpec
  { render = render
  , handleEvent = handleEvent
  }
  where
  input :: Input -> S.Input State
  input { items } =
    { inputType: S.Toggle
    , search: Nothing
    , debounceTime: Nothing
    , getItemCount: length <<< _.items
    , items
    , selection: Nothing
    }

  handleEvent :: S.Event -> H.HalogenM (S.State State) S.Action' () Message Aff Unit
  handleEvent = case _ of
    S.Selected ix -> do
      st <- H.get
      let selection = st.items !! ix
      H.modify_ _ { selection = selection, visibility = S.Off }
      H.raise $ SelectionChanged st.selection selection
    _ -> pure unit

  render :: S.State State -> H.ComponentHTML S.Action' () Aff
  render st =
    HH.div
      [ class_ "geocode-city__dropdown" ]
      [ renderContainer ]
    where
    renderContainer = whenElem (st.visibility == S.On) \_ ->
      HH.div
        ( SS.setContainerProps [ class_ "geocode-city__dropdown-container" ] )
        ( renderItem `mapWithIndex` st.items )
      where
      renderItem index item =
        HH.div
          ( SS.setItemProps index
              [ classes_
                  [ "geocode-city__dropdown-item"
                  , "geocode-city__dropdown-item-highlighted" # guard (st.highlightedIndex == Just index)
                  ]
              ]
          )
          [ HH.text (cityString item) ]

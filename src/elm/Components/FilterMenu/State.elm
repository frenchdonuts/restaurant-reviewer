module Components.FilterMenu.State exposing (..)

import Components.FilterMenu.Types exposing (..)


init : State
init =
    State
        { cuisine = NoPreference
        , openNow = False
        , price = Fancy
        }


toggleOpenNow : State -> State
toggleOpenNow (State state) =
    State { state | openNow = not state.openNow }


setCuisine : Cuisine -> State -> State
setCuisine cuisine (State state) =
    State { state | cuisine = cuisine }


setPrice : Price -> State -> State
setPrice price (State state) =
    State { state | price = price }

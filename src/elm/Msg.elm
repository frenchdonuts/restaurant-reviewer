module Msg exposing (..)

import Types exposing (..)
import Geolocation
import Material
import Components.Autocomplete as Autocomplete


type Msg
    = NoOp
      -- Initialization
    | OnInitErr String
    | OnInitSuc Geolocation.Location
      -- API Restaurant Request
    | FetchRestaurants
    | OnFetchRestaurantsErr String
    | OnFetchRestaurantsSuc (List Restaurant)
      -- Cuisine Selector (Autocomplete)
    | CuisineAutocomplete Autocomplete.Msg
      -- Price Selector
    | ToggleCasual
    | ToggleFancy
      -- Open now toggle
    | ToggleOpenNow
      -- Restaurant List
    | OnRestaurantClick Restaurant
    | Mdl (Material.Msg Msg)

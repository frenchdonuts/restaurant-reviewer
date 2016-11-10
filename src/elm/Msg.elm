module Msg exposing (..)

import Types exposing (..)
import Components.Dropdown.Types as Dropdown
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
    | PriceDropdown Dropdown.Msg
      -- Restaurant List
    | OnRestaurantClick Restaurant
    | Mdl (Material.Msg Msg)

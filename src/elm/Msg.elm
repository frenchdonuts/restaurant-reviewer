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
      -- Restaurants API Request
    | FetchRestaurants
    | OnFetchRestaurantsErr String
    | OnFetchRestaurantsSuc (List RestaurantPreview)
      -- Restaurant API Response
    | OnFetchRestaurantErr String
    | OnFetchRestaurantSuc Restaurant
      -- Cuisine Selector (Autocomplete)
    | CuisineAutocomplete Autocomplete.Msg
      -- Price Selector
    | ToggleCasual
    | ToggleFancy
      -- Open now toggle
    | ToggleOpenNow
      -- Restaurant List
    | OnRestaurantClick RestaurantPreview
    | MouseEnterRestaurantCard (Maybe Int)
    | OnTimezoneOffsetFetched Int
    | Mdl (Material.Msg Msg)

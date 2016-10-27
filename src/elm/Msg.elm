module Msg exposing (..)

import Types exposing (..)
import Geolocation


type Msg
    = OnInitErr String
    | OnInitSuc Geolocation.Location
    | FetchRestaurants
    | OnFetchRestaurantsErr String
    | OnFetchRestaurantsSuc (List Restaurant)
    | OnRestaurantClick Restaurant

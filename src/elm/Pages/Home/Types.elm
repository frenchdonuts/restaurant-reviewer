module Pages.Home.Types exposing (..)

import Components.FilterMenu.Types as FilterMenu
import Geolocation


type alias Model =
    { restaurants : List Restaurant
    , restaurantFilters : FilterMenu.State
    , location : Maybe Geolocation.Location
    , loaderDisplayed : Bool
    , errMsg : String
    }


type alias Restaurant =
    { name : String
    , types : List String
    , photoUrl : String
    , address : String
    }



-- HOW DO I HANDLE ROUTING?
-- I need Msgs to the Router to change the Page to RestaurantPage


type Msg
    = OnInitErr String
    | OnInitSuc Geolocation.Location
    | FetchRestaurants
    | OnFetchRestaurantsErr String
    | OnFetchRestaurantsSuc (List Restaurant)
    | OnRestaurantClick Restaurant

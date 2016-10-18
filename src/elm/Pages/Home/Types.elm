module Pages.Home.Types exposing (..)

import Http
import Geolocation


type alias Model =
    { restaurants : List Restaurant
    , restaurantFilters : String
    , location : ( Float, Float )
    , loaderDisplayed : Bool
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
    | OnInitSuc (List Restaurant)
    | OnRestaurantClick Restaurant

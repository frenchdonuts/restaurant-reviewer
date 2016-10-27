module Model exposing (..)

import Types exposing (..)
import Geolocation


type alias Model =
    { restaurants : List Restaurant
    , restaurantFilters : Filters
    , location : Maybe Geolocation.Location
    , loaderDisplayed : Bool
    , errMsg : String
    }



{-
   type alias Model =
       { data : Data
       , viewModel : ViewModel
       }

   type alias Data =
       { restaurants : List Restaurant
       , location : Maybe Geolocation.Location
       }


   type alias ViewModel =
       { restaurantFilters : Filters
       , loaderDisplayed : Bool
       , errMsg : String
       }
-}

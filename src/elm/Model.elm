module Model exposing (..)

import Types exposing (..)
import Geolocation
import Material
import Components.Autocomplete as Autocomplete
import Dict


type alias Model =
    { restaurants : List RestaurantPreview
    , location : Maybe Geolocation.Location
    , loaderDisplayed : Bool
    , errMsg : String
    , cuisineAutocomplete : Autocomplete.State Cuisine
    , selectedCuisine : Maybe Cuisine
    , menuOpen : Bool
    , indexOfMousedMenuItem : Maybe Int
    , priceFilter : PriceFilter
    , includeOnlyOpenRestaurants : Bool
    , indexOfElevatedCard : Maybe Int
    , selectedRestaurant : Maybe Restaurant
    , newReviews : Dict.Dict String (List NewReview)
    , newReview : NewReview
    , history : List (Maybe Page)
    , mdl : Material.Model
    , timezoneOffset : Int
    }

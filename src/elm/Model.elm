module Model exposing (..)

import Types exposing (..)
import Geolocation
import Material
import Components.Autocomplete as Autocomplete


type alias Model =
    { restaurants : List RestaurantPreview
    , location : Maybe Geolocation.Location
    , loaderDisplayed : Bool
    , errMsg : String
    , cuisineAutocomplete : Autocomplete.State Cuisine
    , includeCasualInSearch : Bool
    , includeFancyInSearch : Bool
    , openNow : Bool
    , indexOfElevatedCard : Maybe Int
    , mdl : Material.Model
    }

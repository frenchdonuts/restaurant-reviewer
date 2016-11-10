module Model exposing (..)

import Types exposing (..)
import Components.Dropdown.Types as Dropdown
import Geolocation
import Material
import Components.Autocomplete as Autocomplete


type alias Model =
    { restaurants : List Restaurant
    , restaurantFilters : Filters
    , location : Maybe Geolocation.Location
    , loaderDisplayed : Bool
    , errMsg : String
    , cuisineAutocomplete : Autocomplete.State Cuisine
    , priceDropdown : Dropdown.State
    , mdl : Material.Model
    }

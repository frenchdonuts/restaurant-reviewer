module Update exposing (init, subscriptions, update)

import Types exposing (..)
import Model exposing (..)
import Msg exposing (..)
import Api exposing (..)
import Components.Dropdown.State as Dropdown
import Helper exposing (cuisines, cuisineString, cuisineStringInverse, prices)
import Task exposing (Task, perform, onError)
import Geolocation exposing (now)
import Material
import Components.Autocomplete as Autocomplete


init : ( Model, Cmd Msg )
init =
    ( { restaurants = []
      , restaurantFilters = initFilters
      , location = Nothing
      , loaderDisplayed = True
      , errMsg = ""
      , mdl = Material.model
      , cuisineAutocomplete = Autocomplete.init
      , priceDropdown = Dropdown.init (List.length prices)
      }
    , Task.perform OnInitErr OnInitSuc initTask
    )


initFilters : Filters
initFilters =
    { cuisine = NoPreference
    , openNow = False
    , maxPrice = Casual
    }


initTask : Task String Geolocation.Location
initTask =
    now `onError` (\_ -> Task.fail "Geolocation failed.")


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map CuisineAutocomplete Autocomplete.subscription


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        { cuisineAutocomplete, priceDropdown } =
            model
    in
        case msg of
            NoOp ->
                ( model, Cmd.none )

            OnInitErr err ->
                ( { model | errMsg = err }, Cmd.none )

            OnInitSuc location ->
                ( { model | location = Just location }
                , Task.perform
                    OnFetchRestaurantsErr
                    OnFetchRestaurantsSuc
                    (fetchRestaurants model)
                )

            FetchRestaurants ->
                ( model
                , Task.perform
                    OnFetchRestaurantsErr
                    OnFetchRestaurantsSuc
                    (fetchRestaurants model)
                )

            OnFetchRestaurantsErr errMsg ->
                ( { model
                    | errMsg = errMsg
                    , loaderDisplayed = False
                  }
                , Cmd.none
                )

            OnFetchRestaurantsSuc restaurants ->
                ( { model
                    | restaurants = restaurants
                    , loaderDisplayed = False
                  }
                , Cmd.none
                )

            -- Routing happens here
            OnRestaurantClick restaurant ->
                ( model, Cmd.none )

            -- Cuisine Selector (Autocomplete)
            CuisineAutocomplete msg ->
                let
                    ( newState, cmd ) =
                        Autocomplete.update cuisineAutocompleteUpdateConfig msg cuisineAutocomplete cuisines

                    newModel =
                        { model | cuisineAutocomplete = newState }
                in
                    newModel ! [ Cmd.map CuisineAutocomplete cmd ]

            -- Price Selector
            PriceDropdown msg ->
                let
                    ( priceDropdown', cmd ) =
                        Dropdown.update msg priceDropdown
                in
                    ( { model
                        | priceDropdown = priceDropdown'
                      }
                    , Cmd.map PriceDropdown cmd
                    )

            Mdl msg ->
                Material.update msg model


fetchRestaurants : Model -> Task String (List Restaurant)
fetchRestaurants { location, restaurantFilters } =
    case location of
        Just location ->
            getRestaurants location.latitude location.longitude restaurantFilters
                `onError` (\_ -> Task.fail "Failed to fetch restaurants.")

        Nothing ->
            Task.fail "Location failed to fetch."


cuisineAutocompleteUpdateConfig : Autocomplete.UpdateConfig Cuisine
cuisineAutocompleteUpdateConfig =
    { toId = cuisineString }

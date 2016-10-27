module Update exposing (init, update)

import Types exposing (..)
import Model exposing (..)
import Msg exposing (..)
import Api exposing (..)
import Task exposing (Task, perform, onError)
import Geolocation exposing (now)
import Debug


init : ( Model, Cmd Msg )
init =
    Debug.log "init"
        ( { restaurants = []
          , restaurantFilters = initFilters
          , location = Nothing
          , loaderDisplayed = True
          , errMsg = ""
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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


fetchRestaurants : Model -> Task String (List Restaurant)
fetchRestaurants { location, restaurantFilters } =
    case location of
        Just location ->
            getRestaurants location.latitude location.longitude restaurantFilters
                `onError` (\_ -> Task.fail "Failed to fetch restaurants.")

        Nothing ->
            Task.fail "Location failed to fetch."

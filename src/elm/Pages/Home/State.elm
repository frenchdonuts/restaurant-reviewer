module Pages.Home.State exposing (..)

import Pages.Home.Types exposing (..)
import Pages.Home.Api exposing (..)
import Components.FilterMenu.State as FilterMenu
import Task exposing (Task, succeed, andThen, onError)
import Geolocation exposing (now)


init : ( Model, Cmd Msg )
init =
    ( { restaurants = []
      , restaurantFilters = FilterMenu.init
      , location = Nothing
      , loaderDisplayed = True
      , errMsg = ""
      }
    , Task.perform OnInitErr OnInitSuc initialTask
    )


initialTask : Task String Geolocation.Location
initialTask =
    now `onError` (\_ -> Task.fail "Geolocation failed.")


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnInitErr err ->
            ( { model | errMsg = err }, Cmd.none )

        OnInitSuc location ->
            ( { model | location = Just location }, Cmd.none )

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

module Pages.Home.State exposing (..)

import Pages.Home.Types exposing (..)
import Pages.Home.Api exposing (..)
import Task exposing (Task, succeed, andThen, onError)
import Geolocation exposing (now)


init : ( Model, Cmd Msg )
init =
    ( { restaurants = []
      , restaurantFilters = ""
      , location = ( 0.0, 0.0 )
      , loaderDisplayed = True
      }
    , Task.perform OnInitErr OnInitSuc initialTask
    )


initialTask : Task String (List Restaurant)
initialTask =
    let
        getCurrentLocation =
            now `onError` (\_ -> Task.fail "Geolocation failed.")

        fetchRestaurants location =
            getRestaurants location.latitude location.longitude
                `onError` (\_ -> Task.fail "Failed to fetch restaurants.")
    in
        getCurrentLocation `andThen` fetchRestaurants


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnInitErr err ->
            ( model, Cmd.none )

        OnInitSuc restaurants ->
            ( { model | restaurants = restaurants }, Cmd.none )

        OnRestaurantClick restaurant ->
            ( model, Cmd.none )

module Update exposing (init, subscriptions, update)

import Types exposing (..)
import Model exposing (..)
import Msg exposing (..)
import Api exposing (..)
import Helper exposing (cuisines, cuisineString, cuisineStringInverse, prices)
import Components.Autocomplete as Autocomplete
import Task exposing (Task, perform, onError)
import Geolocation exposing (now)
import Material
import Material.Layout as Layout


init : ( Model, Cmd Msg )
init =
    { restaurants = []
    , location = Nothing
    , loaderDisplayed = True
    , errMsg = ""
    , mdl = Material.model
    , cuisineAutocomplete = Autocomplete.init
    , includeCasualInSearch = True
    , includeFancyInSearch = True
    , openNow = False
    }
        ! [ Task.perform OnInitErr OnInitSuc initTask, Layout.sub0 Mdl ]


initTask : Task String Geolocation.Location
initTask =
    now `onError` (\_ -> Task.fail "Geolocation failed.")


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map CuisineAutocomplete Autocomplete.subscription
        , Layout.subs Mdl model.mdl
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        { cuisineAutocomplete } =
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

            -- Price Selector - make sure at least one of them is always True
            ToggleCasual ->
                let
                    casual =
                        not model.includeCasualInSearch

                    fancy =
                        if not casual then
                            True
                        else
                            model.includeFancyInSearch
                in
                    ( { model
                        | includeCasualInSearch = casual
                        , includeFancyInSearch = fancy
                      }
                    , Cmd.none
                    )

            ToggleFancy ->
                let
                    fancy =
                        not model.includeFancyInSearch

                    casual =
                        if not fancy then
                            True
                        else
                            model.includeCasualInSearch
                in
                    ( { model
                        | includeFancyInSearch = fancy
                        , includeCasualInSearch = casual
                      }
                    , Cmd.none
                    )

            ToggleOpenNow ->
                { model | openNow = not model.openNow } ! []

            Mdl msg ->
                Material.update msg model


fetchRestaurants : Model -> Task String (List Restaurant)
fetchRestaurants model =
    case model.location of
        Just location ->
            getRestaurants location.latitude location.longitude model
                `onError` (\_ -> Task.fail "Failed to fetch restaurants.")

        Nothing ->
            Task.fail "Location failed to fetch."


cuisineAutocompleteUpdateConfig : Autocomplete.UpdateConfig Cuisine
cuisineAutocompleteUpdateConfig =
    { toId = cuisineString }

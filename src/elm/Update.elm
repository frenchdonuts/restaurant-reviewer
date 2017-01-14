port module Update exposing (init, subscriptions, update)

import Types exposing (..)
import Model exposing (..)
import Msg exposing (..)
import Api exposing (..)
import Nav
import Helper exposing (cuisines, maybeToCuisine, cuisineToMaybe, cuisineString, cuisineStringInverse, prices, intToRating, (>>=))
import Utils exposing (isJust)
import Zipper1D as Zipper
import Components.Autocomplete as Autocomplete
import Configs
import Http
import Task exposing (..)
import String
import Maybe
import Dict
import Navigation
import Geolocation exposing (now)
import Material
import Material.Layout as Layout
import Time.DateTime as Time
import Time as CoreTime
import UrlParser as Url
import Maybe.Extra exposing (or)


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    { restaurants = []
    , location = Nothing
    , loaderDisplayed = True
    , errMsg = ""
    , shouldAlert = False
    , mdl = Material.model
    , cuisineAutocomplete = Autocomplete.init "cuisine"
    , selectedCuisine = Nothing
    , menuOpen = False
    , indexOfMousedMenuItem = Nothing
    , includeCasualInSearch = True
    , includeFancyInSearch = True
    , openNow = False
    , indexOfElevatedCard = Nothing
    , selectedRestaurant = Nothing
    , newReview = initNewReview
    , newReviews = Dict.empty
    , timezoneOffset = 0
    , history = [ Url.parsePath Nav.route location ]
    }
        ! [ Task.attempt Initialized initTask, Layout.sub0 Mdl ]


initTask : Task String Geolocation.Location
initTask =
    now |> onError (\_ -> Task.fail "Geolocation failed.")


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map CuisineAutocomplete Autocomplete.subscription
        , Layout.subs Mdl model.mdl
        , setTimezoneOffset OnTimezoneOffsetFetched
        ]


port setTimezoneOffset : (Int -> msg) -> Sub msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        { cuisineAutocomplete, selectedRestaurant, newReview, errMsg } =
            model
    in
        case msg of
            NoOp ->
                ( model, Cmd.none )

            Initialized initializationAttemptResult ->
                case initializationAttemptResult of
                    Ok location ->
                        ( { model | location = Just location }
                        , Cmd.none
                        )

                    Err err ->
                        ( { model | errMsg = err }, Cmd.none )

            FetchRestaurants ->
                let
                    cmd =
                        case model.location of
                            Just location ->
                                Http.send FetchedRestaurants <|
                                    getRestaurants location.latitude location.longitude selectedCuisine model

                            Nothing ->
                                Debug.log "No location" Cmd.none

                    selectedCuisine =
                        List.filter cuisineFilter cuisines
                            |> List.head
                            |> or model.selectedCuisine
                            |> maybeToCuisine
                            |> Debug.log "selectedCuisine"

                    autocompleteQuery =
                        String.toLower <| Autocomplete.getQuery cuisineAutocomplete

                    maybeCuisine =
                        List.filter cuisineFilter cuisines
                            |> List.head

                    cuisineFilter =
                        cuisineString
                            >> String.toLower
                            >> (==) autocompleteQuery
                in
                    model ! [ cmd ]

            FetchedRestaurants fetchRestaurantsResult ->
                case fetchRestaurantsResult of
                    Ok restaurants ->
                        let
                            restaurants_ =
                                if model.openNow then
                                    List.filter ((==) True << Utils.maybeToBool << .openNow) restaurants
                                else
                                    restaurants
                        in
                            ( { model
                                | restaurants = restaurants_
                                , loaderDisplayed = False
                              }
                            , Cmd.none
                            )

                    Err httpError ->
                        ( { model
                            | errMsg = toString httpError
                            , loaderDisplayed = False
                          }
                        , Cmd.none
                        )

            FetchedRestaurant fetchRestaurantResult ->
                case fetchRestaurantResult of
                    Ok restaurant ->
                        ( { model
                            | selectedRestaurant = Just restaurant
                            , loaderDisplayed = False
                          }
                        , Cmd.none
                        )

                    Err httpError ->
                        ( { model
                            | errMsg = Debug.log "fetch restaurant error" httpError |> (\_ -> "Error fetching restaurant")
                            , loaderDisplayed = False
                          }
                        , Cmd.none
                        )

            -- Routing happens here
            OnRestaurantClick restaurantPreview ->
                let
                    newPage =
                        RestaurantDetail restaurantPreview.id
                in
                    model
                        ! [ Navigation.newUrl <| Nav.toPath newPage
                          , Http.send FetchedRestaurant (getRestaurant restaurantPreview.id)
                          ]

            -- Cuisine Selector (Autocomplete)
            CuisineAutocomplete msg ->
                let
                    ( newState, cmd, maybeMsg ) =
                        Autocomplete.update Configs.cuisineAutocompleteUpdateConfig msg cuisineAutocomplete cuisines

                    newModel =
                        { model | cuisineAutocomplete = newState }

                    ( newModel_, cmd_ ) =
                        case maybeMsg of
                            Just msg ->
                                update msg newModel

                            Nothing ->
                                ( newModel, Cmd.none )
                in
                    newModel_ ! [ Cmd.map CuisineAutocomplete cmd, cmd_ ]

            SelectedCuisine cuisine ->
                let
                    errMsg =
                        "Please choose a cuisine type from the list"
                in
                    { model
                        | selectedCuisine = cuisine
                        , errMsg = Maybe.Extra.unwrap errMsg (\_ -> "") cuisine
                    }
                        ! []

            OnSearchBtnPressed ->
                let
                    ( newModel, cmd ) =
                        if not <| String.isEmpty errMsg then
                            update (AlertAccessibilityUser True) model
                        else
                            update FetchRestaurants model
                in
                    newModel ! [ cmd ]

            AlertAccessibilityUser shouldAlert ->
                { model | shouldAlert = shouldAlert } ! []

            {--Filter Menu --}
            ToggleMenu ->
                ( { model | menuOpen = not model.menuOpen }, Cmd.none )

            MouseEnterMenuItem maybeIndex ->
                { model | indexOfMousedMenuItem = maybeIndex } ! []

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

            MouseEnterRestaurantCard maybeIndex ->
                { model | indexOfElevatedCard = maybeIndex } ! []

            PrevPhoto ->
                let
                    selectedRestaurant_ =
                        Maybe.map (\restaurant -> { restaurant | photos = Zipper.backward restaurant.photos }) selectedRestaurant
                in
                    { model | selectedRestaurant = selectedRestaurant_ } ! []

            NextPhoto ->
                let
                    selectedRestaurant_ =
                        Maybe.map (\restaurant -> { restaurant | photos = Zipper.forward restaurant.photos }) selectedRestaurant
                in
                    { model | selectedRestaurant = selectedRestaurant_ } ! []

            OnUpdateNewReview msg ->
                let
                    ( newReview_, cmd ) =
                        case msg of
                            UpdateName name ->
                                { newReview | authorName = name } ! []

                            UpdateTime dateTime ->
                                { newReview | time = Just dateTime } ! []

                            UpdateRating rating ->
                                { newReview | rating = rating } ! []

                            UpdateText text ->
                                { newReview | text = text } ! []
                in
                    { model | newReview = newReview_ } ! []

            OnNewReviewSubmitBtnPressed ->
                let
                    selectedRestaurantId =
                        Maybe.map (.id) model.selectedRestaurant
                            |> Maybe.withDefault
                                (Debug.log
                                    "ERR: Inconsistent state. We are trying to add a NewReview but we have not selected a Restaurant to associate it with"
                                    ""
                                )

                    validateNewReview { authorName, text } =
                        (authorName /= "")
                            && (text /= "")
                in
                    if validateNewReview newReview then
                        model ! [ Task.perform (ValidNewReviewSubmitted newReview) CoreTime.now ]
                    else
                        { model
                            | newReview = Debug.log "Invalid NewReview" newReview
                        }
                            ! []

            ValidNewReviewSubmitted newReview time ->
                let
                    selectedRestaurantId =
                        Maybe.map (.id) model.selectedRestaurant
                            |> Maybe.withDefault
                                (Debug.log
                                    "ERR: Inconsistent state. We are trying to add a NewReview but we have not selected a Restaurant to associate it with"
                                    ""
                                )

                    newReviewWithTime =
                        { newReview | time = Just (Time.fromTimestamp time) }

                    addNewReviewToRestaurant id newReview_ dict =
                        if Dict.member id dict then
                            Dict.update id (Maybe.map (\listOfNewReviews -> newReview_ :: listOfNewReviews)) dict
                        else
                            Dict.insert id [ newReview_ ] dict
                in
                    { model
                        | newReview = initNewReview
                        , newReviews = addNewReviewToRestaurant selectedRestaurantId newReviewWithTime model.newReviews
                    }
                        ! []

            UrlChange navLocation ->
                { model | history = Url.parsePath Nav.route navLocation :: model.history }
                    ! []

            OnTimezoneOffsetFetched offsetInMin ->
                { model | timezoneOffset = offsetInMin } ! []

            Mdl msg ->
                Material.update msg model

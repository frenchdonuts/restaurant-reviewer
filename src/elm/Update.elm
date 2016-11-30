port module Update exposing (init, subscriptions, update, mockRestaurantDetailInit)

import Types exposing (..)
import Model exposing (..)
import Msg exposing (..)
import Api exposing (..)
import Nav
import Helper exposing (cuisines, cuisineString, cuisineStringInverse, prices, intToRating, isJust)
import Zipper1D as Zipper
import Components.Autocomplete as Autocomplete
import Task exposing (..)
import Maybe
import Dict
import Navigation
import Geolocation exposing (now)
import Material
import Material.Layout as Layout
import Time.DateTime as Time
import Time as CoreTime


init : ( Model, Cmd Msg )
init =
    { currentPage = Home
    , restaurants = []
    , location = Nothing
    , loaderDisplayed = True
    , errMsg = ""
    , mdl = Material.model
    , cuisineAutocomplete = Autocomplete.init "cuisine"
    , includeCasualInSearch = True
    , includeFancyInSearch = True
    , openNow = False
    , indexOfElevatedCard = Nothing
    , selectedRestaurant = Nothing
    , newReview = initNewReview
    , newReviews = Dict.empty
    , timezoneOffset = 0
    }
        ! [ Task.perform OnInitErr OnInitSuc initTask, Layout.sub0 Mdl ]


mockRestaurantDetailInit : ( Model, Cmd Msg )
mockRestaurantDetailInit =
    let
        initialModel =
            { currentPage = RestaurantDetail ""
            , restaurants = []
            , location = Nothing
            , loaderDisplayed = True
            , errMsg = ""
            , mdl = Material.model
            , cuisineAutocomplete = Autocomplete.init "cuisine"
            , includeCasualInSearch = True
            , includeFancyInSearch = True
            , openNow = False
            , indexOfElevatedCard = Nothing
            , selectedRestaurant = Nothing
            , newReview = initNewReview
            , newReviews = Dict.empty
            , timezoneOffset = 0
            }
    in
        initialModel
            ! [ Task.perform
                    OnFetchRestaurantErr
                    OnFetchRestaurantSuc
                    (mockRestaurantDetailInitTask initialModel)
              ]


mockRestaurantDetailInitTask : Model -> Task String Restaurant
mockRestaurantDetailInitTask model =
    let
        findOutWhereWeAre =
            Debug.log "Finding out where we are " now
                `onError` (\err -> Debug.log "Fetch current location error." err |> (\_ -> fail "Couldn't find out where we are."))

        getSomeRestaurants location =
            Debug.log "Getting some restaurants" <|
                getRestaurants location.latitude location.longitude model
                    `onError` (\err -> Debug.log "Fetch restaurants error" err |> (\_ -> fail "Couldn't fetch any restaurants."))

        extractSomeRestaurant restaurants =
            case List.head restaurants of
                Just restaurant ->
                    succeed restaurant

                Nothing ->
                    fail <| Debug.log "" "No restaurants around here bruh."

        getDetailsAboutThatRestaurant restaurant =
            Debug.log "Getting the restaurant deets" <|
                getRestaurant restaurant.id
                    `onError` (\err -> Debug.log "Fetch restaurant details error" err |> (\_ -> fail "Trouble fetching restaurant details."))
    in
        findOutWhereWeAre
            `andThen` getSomeRestaurants
            `andThen` extractSomeRestaurant
            `andThen` getDetailsAboutThatRestaurant


initTask : Task String Geolocation.Location
initTask =
    now `onError` (\_ -> Task.fail "Geolocation failed.")


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
        { cuisineAutocomplete, selectedRestaurant, newReview } =
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

            OnFetchRestaurantErr httpErr ->
                ( { model
                    | errMsg = Debug.log "fetch restaurant error" httpErr |> (\_ -> "Error fetching restaurant")
                    , loaderDisplayed = False
                  }
                , Cmd.none
                )

            OnFetchRestaurantSuc restaurant ->
                ( { model
                    | selectedRestaurant = Just restaurant
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
                    { model | currentPage = newPage }
                        ! [ Navigation.newUrl <| Nav.toPath newPage
                          , Task.perform
                                OnFetchRestaurantErr
                                OnFetchRestaurantSuc
                                (Task.mapError (\err -> Debug.log "fetch restaurant error" err |> (\_ -> "Couldn't fetch restaurant")) <| getRestaurant restaurantPreview.id)
                          ]

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

            MouseEnterRestaurantCard maybeIndex ->
                { model | indexOfElevatedCard = maybeIndex } ! []

            PrevPhoto ->
                let
                    selectedRestaurant' =
                        Maybe.map (\restaurant -> { restaurant | photos = Zipper.backward restaurant.photos }) selectedRestaurant
                in
                    { model | selectedRestaurant = selectedRestaurant' } ! []

            NextPhoto ->
                let
                    selectedRestaurant' =
                        Maybe.map (\restaurant -> { restaurant | photos = Zipper.forward restaurant.photos }) selectedRestaurant
                in
                    { model | selectedRestaurant = selectedRestaurant' } ! []

            OnUpdateNewReview msg ->
                let
                    ( newReview', cmd ) =
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
                    { model | newReview = newReview' } ! []

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
                        model ! [ Task.perform (\_ -> NoOp) (ValidNewReviewSubmitted newReview) CoreTime.now ]
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

                    addNewReviewToRestaurant id newReview' dict =
                        if Dict.member id dict then
                            Dict.update id (Maybe.map (\listOfNewReviews -> newReview' :: listOfNewReviews)) dict
                        else
                            Dict.insert id [ newReview' ] dict
                in
                    { model
                        | newReview = initNewReview
                        , newReviews = addNewReviewToRestaurant selectedRestaurantId newReviewWithTime model.newReviews
                    }
                        ! []

            OnTimezoneOffsetFetched offsetInMin ->
                { model | timezoneOffset = Debug.log "offsetInMs" offsetInMin } ! []

            Mdl msg ->
                Material.update msg model


fetchRestaurants : Model -> Task String (List RestaurantPreview)
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

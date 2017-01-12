port module Update exposing (init, subscriptions, update)

import Types exposing (..)
import Model exposing (..)
import Msg exposing (..)
import Api exposing (..)
import Nav
import Helper exposing (cuisines, maybeToCuisine, cuisineToMaybe, cuisineString, cuisineStringInverse, prices, intToRating)
import Utils exposing (isJust)
import Zipper1D as Zipper
import Components.Autocomplete as Autocomplete
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
    , mdl = Material.model
    , cuisineAutocomplete = Autocomplete.init "cuisine"
    , selectedCuisine = NoPreference
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



{-
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
                   |> onError (\err -> Debug.log "Fetch current location error." err |> (\_ -> fail "Couldn't find out where we are."))

           getSomeRestaurants location =
               getRestaurants location.latitude location.longitude model
                   |> Debug.log "Getting some restaurants"
                   |> onError (\err -> Debug.log "Fetch restaurants error" err |> (\_ -> fail "Couldn't fetch any restaurants."))

           extractSomeRestaurant restaurants =
               case List.head restaurants of
                   Just restaurant ->
                       succeed restaurant

                   Nothing ->
                       fail <| Debug.log "" "No restaurants around here bruh."

           getDetailsAboutThatRestaurant restaurant =
               getRestaurant restaurant.id
                   |> Debug.log "Getting the restaurant deets"
                   |> onError (\err -> Debug.log "Fetch restaurant details error" err |> (\_ -> fail "Trouble fetching restaurant details."))
       in
           findOutWhereWeAre
               |> andThen getSomeRestaurants
               |> andThen extractSomeRestaurant
               |> andThen getDetailsAboutThatRestaurant
-}


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
        { cuisineAutocomplete, selectedRestaurant, newReview } =
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
                    autocompleteQuery =
                        String.toLower <| Autocomplete.getQuery cuisineAutocomplete

                    -- TODO: Figure out why autocompleteQuery is still equal to the TYPED query even after User CLICKS on dropdown item
                    -- Use elm debugger
                    maybeCuisine =
                        (cuisineString >> String.toLower >> (==) autocompleteQuery)
                            |> flip List.filter cuisines
                            |> List.head

                    cuisineFilter =
                        cuisineString
                            >> String.toLower
                            >> (==) autocompleteQuery

                    selectedCuisine =
                        List.filter cuisineFilter cuisines
                            |> List.head
                            |> or (cuisineToMaybe model.selectedCuisine)
                            |> maybeToCuisine

                    log =
                        Debug.log "selectedCuisine" selectedCuisine

                    cmd =
                        case model.location of
                            Just location ->
                                Http.send FetchedRestaurants <|
                                    getRestaurants location.latitude location.longitude selectedCuisine model

                            Nothing ->
                                Debug.log "No location" Cmd.none
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
                        Autocomplete.update cuisineAutocompleteUpdateConfig msg cuisineAutocomplete cuisines

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
                { model | selectedCuisine = cuisine } ! []

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
                Debug.log "Navigating..." <|
                    { model | history = Url.parsePath Nav.route navLocation :: model.history }
                        ! []

            OnTimezoneOffsetFetched offsetInMin ->
                { model | timezoneOffset = Debug.log "offsetInMs" offsetInMin } ! []

            Mdl msg ->
                Material.update msg model


cuisineAutocompleteUpdateConfig : Autocomplete.UpdateConfig Msg Cuisine
cuisineAutocompleteUpdateConfig =
    { toId = cuisineString
    , onSelectChoice =
        SelectedCuisine
            << (\maybeCuisine ->
                    case maybeCuisine of
                        Just cuisine ->
                            cuisine

                        Nothing ->
                            NoPreference
               )
    }

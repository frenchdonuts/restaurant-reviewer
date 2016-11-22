module Views.Pages.RestaurantDetail exposing (view)

import Model exposing (Model)
import Types exposing (Restaurant, Review, Rating(..), Period)
import Views.Helpers exposing (ratingToInt, ratingToString, dayTimeToString, dayToString, periodToString)
import Msg exposing (..)
import Html exposing (Html, div, text, img)
import Html.Attributes as Attrs
import Html.Events as Events
import Material.Grid exposing (grid, cell, size, offset, Device(..))
import Material.Textfield as Textfield
import Material.List as List
import Material.Button as Button
import Material.Options as Options exposing (css)
import Material.Color as Color
import Material.Icon as Icon
import Material.Elevation as Elevation
import Material.Typography as Typography
import Time.DateTime as Time


view : Model -> Html Msg
view model =
    let
        { selectedRestaurant } =
            model

        view =
            case selectedRestaurant of
                Just restaurant ->
                    restaurantView restaurant model

                Nothing ->
                    div [] [ text "No restaurant selected" ]
    in
        view


restaurantView : Restaurant -> Model -> Html Msg
restaurantView r m =
    let
        sections =
            [ header r m
            , newReview r m
            , userReviews r m
            ]

        view =
            grid [] << List.map (\section -> cell [ size All 12, css "min-height" "33%", css "border" (Color.hueName Color.Grey) ] [ section ])
    in
        grid []
            [ cell [ size Desktop 10, offset Desktop 1, size Tablet 8, size Phone 4 ]
                [ (Options.styled' div)
                    [ css "position" "relative"
                    , css "min-height" "800px"
                      --, css "margin" "4% 24% 4% 24%"
                    , Elevation.e2
                    ]
                    []
                    [ header r m
                    , newReviewAndCurrentRating r m
                    , userReviews r m
                    ]
                ]
            ]


{-| At least one image of the restaurant here (maybe even a carousel?)
    Lower left corner:
        Header: Restaurant Name
        Sub-header: Restaurant address
        Sub-header: Hours open today
-}
header : Restaurant -> Model -> Html Msg
header r m =
    let
        { id, name, photos, address, reviews, avgRating, openingHours } =
            r

        backgroundPhoto =
            case List.head photos of
                Just url ->
                    css "background-image" ("url('" ++ url ++ "') ")

                Nothing ->
                    css "background-image" "url('assets/pomegranate.jpg')"

        openingHoursToday =
            case openingHours of
                Just periods ->
                    case List.head periods of
                        Just period ->
                            periodToString period

                        Nothing ->
                            "Closed"

                Nothing ->
                    "Hours not available."

        title string =
            Options.div
                [ Typography.title
                , Typography.contrast 1.0
                , css "margin-top" "24px"
                , css "margin-left" "12px"
                ]
                [ text string ]

        subhead string =
            Options.div
                [ Typography.subhead
                , Typography.contrast 1.0
                , css "margin-left" "10px"
                ]
                [ text string ]
    in
        Options.div
            [ css "min-height" "300px"
            , css "background-image" "url('static/img/q_restaurant.jpg')"
              -- backgroundPhoto
            , css "background-color" "black"
            , css "background-repeat" "no-repeat"
            , css "background-position" "center"
            , css "position" "relative"
            ]
            [ Options.div
                [ css "width" "100%"
                , css "height" "100px"
                , Options.scrim 0.75
                , css "position" "absolute"
                , css "bottom" "0"
                , Color.text Color.white
                ]
                [ title name
                , subhead address
                , subhead openingHoursToday
                ]
            ]


{-|
-}
hoursToday : Period -> Html Msg
hoursToday period =
    let
        { open, close } =
            period

        closingTime =
            case close of
                Just dayTime ->
                    dayTimeToString dayTime

                Nothing ->
                    "Never closes"
    in
        div
            []
            [ text <| "Open: " ++ dayTimeToString open
            , text <| "Close: " ++ closingTime
            ]


{-|
-}
newReviewAndCurrentRating : Restaurant -> Model -> Html Msg
newReviewAndCurrentRating r m =
    grid
        [ css "min-height" "300px" ]
        [ cell
            [ size All 6 ]
            [ newReview r m ]
        , cell
            [ size All 6
            , css "padding" "5% 0"
            ]
            [ sexyRating r m ]
        ]


{-|
-}
newReview : Restaurant -> Model -> Html Msg
newReview r m =
    grid
        [ --css "padding" "15px 8% 0 8%"
          css "min-height" "300px"
        ]
        [ cell [ size All 12 ]
            [ Textfield.render
                Mdl
                [ 1 ]
                m.mdl
                [ Textfield.label "Name "
                , Textfield.text'
                , css "width" "100%"
                ]
            ]
        , cell [ size All 12 ]
            [ Textfield.render Mdl
                [ 0 ]
                m.mdl
                [ Textfield.label "Your review"
                  --, Textfield.floatingLabel
                , Textfield.textarea
                , Textfield.rows 3
                , css "width" "100%"
                , Options.inner [ css "resize" "none" ]
                ]
            ]
        , cell [ size All 12 ] [ stars 48 Two ]
        , cell [ size All 12 ]
            [ Button.render Mdl
                [ 2 ]
                m.mdl
                [ Button.raised
                , Button.colored
                , css "width" "100%"
                , css "margin-top" "8px"
                ]
                [ text "Add Review" ]
            ]
        ]


stars : Int -> Rating -> Html Msg
stars sizepx r =
    let
        color i =
            if i <= ratingToInt r then
                Color.primary
            else
                Color.color Color.Grey Color.S400

        star i =
            Options.span [ Options.attribute <| Events.onClick NoOp ]
                [ Icon.view
                    "star"
                    [ Color.text (color i)
                    , Typography.contrast 1.0
                    , css "font-size" <| toString sizepx ++ "px"
                    , css "width" "20%"
                    , css "text-align" "center"
                    ]
                ]
    in
        Options.div
            []
            (List.map star [1..5])


{-|
-}
sexyRating : Restaurant -> Model -> Html Msg
sexyRating r m =
    let
        { avgRating } =
            r

        sizepx =
            160
    in
        Options.div
            [ Typography.display4
            , Typography.contrast 1.0
            , css "padding" "10% 0"
            , css "text-align" "center"
            , css "font-size" <| toString sizepx ++ "px"
            ]
            [ Html.text <| (toString avgRating)
            , Icon.view
                "star"
                [ Color.text Color.primary
                , css "font-size" <| toString sizepx ++ "px"
                , css "position" "relative"
                , css "top" "18px"
                ]
            ]


{-|
-}
userReviews : Restaurant -> Model -> Html Msg
userReviews r m =
    let
        { id, name, photos, address, reviews, avgRating, openingHours } =
            r
    in
        List.ul
            []
            (List.map (flip userReview m) reviews)


{-|
-}
userReview : Review -> Model -> Html Msg
userReview review m =
    let
        { authorName, time, rating, text } =
            review

        -- Local Time = UTCTime - Offset = (-Offset) + UTCTime
        localTime =
            Time.addMinutes (negate m.timezoneOffset) time

        formatedLocalTime =
            (localTime |> Time.month >> toString)
                ++ "/"
                ++ (localTime |> Time.day >> toString)
                ++ "/"
                ++ (localTime |> Time.year >> toString)
    in
        List.li
            [ List.withBody
            ]
            [ List.content []
                [ Html.text authorName
                , Options.span [ Typography.caption ] [ Html.text <| " on " ++ formatedLocalTime ]
                , List.body
                    [ css "min-height" "52px"
                    , css "height" "auto"
                    ]
                    [ Options.span
                        [ css "font-weight" "600"
                        , css "margin-right" "8px"
                        ]
                        [ Html.text <| (ratingToString rating)
                        , Icon.view
                            "star"
                            [ Color.text Color.primary
                            , css "position" "relative"
                            , css "top" "5px"
                            ]
                        ]
                    , Options.span [] [ Html.text text ]
                    ]
                ]
            ]

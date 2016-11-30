module Views.Pages.RestaurantDetail exposing (view)

import Model exposing (Model)
import Types exposing (Restaurant, Review, Rating(..), Period)
import Views.Helpers exposing (ratingToString, dayTimeToString, dayToString, periodToString, onKeyUp, toStringf)
import Helper exposing (ratingToInt)
import Msg exposing (..)
import Zipper1D as Zipper
import Html exposing (Html, div, text, img, button)
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
import Dict


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


{-| A set of CSS properties to move an element off-screen
-}
invisible : Html.Attribute msg
invisible =
    Attrs.style [ ( "position", "absolute" ), ( "top", "-999999px" ) ]


restaurantView : Restaurant -> Model -> Html Msg
restaurantView r m =
    let
        view =
            grid [] << List.map (\section -> cell [ size All 12, css "min-height" "33%", css "border" (Color.hueName Color.Grey) ] [ section ])
    in
        grid []
            [ cell [ size Desktop 8, offset Desktop 2, size Tablet 8, size Phone 4 ]
                [ (Options.styled' div)
                    [ css "position" "relative"
                    , css "min-height" "800px"
                    , Elevation.e2
                    ]
                    []
                    [ header r m
                    , newReviewAndCurrentRating r m
                    , Html.h2 [ invisible ] [ Html.text "User Reviews" ]
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

        { src, alt } =
            Zipper.current photos

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

        prevPhotoBtn =
            let
                sizepx =
                    40
            in
                Button.render Mdl
                    [ 5 ]
                    m.mdl
                    [ Button.plain
                    , Button.icon
                    , Button.onClick PrevPhoto
                    , css "position" "absolute"
                    , css "top" "126px"
                    , css "left" "0"
                    , css "width" (toString sizepx ++ "px")
                    , css "height" (toString sizepx ++ "px")
                    ]
                    [ Icon.view "navigate_before"
                        [ css "font-size" (toString sizepx ++ "px")
                        , css "line-height" (toString sizepx ++ "px")
                        , css "transform" ("translate(-" ++ (toString <| sizepx / 2) ++ "px,-" ++ (toString <| sizepx / 2) ++ "px)")
                        , Color.text Color.white
                        ]
                    ]

        nextPhotoBtn =
            let
                sizepx =
                    40
            in
                Button.render Mdl
                    [ 6 ]
                    m.mdl
                    [ Button.plain
                    , Button.icon
                    , Button.onClick NextPhoto
                    , css "position" "absolute"
                    , css "top" "126px"
                    , css "right" "0"
                    , css "width" (toString sizepx ++ "px")
                    , css "height" (toString sizepx ++ "px")
                    ]
                    [ Icon.view "navigate_next"
                        [ css "font-size" (toString sizepx ++ "px")
                        , css "line-height" (toString sizepx ++ "px")
                        , css "transform" ("translate(-" ++ (toString <| sizepx / 2) ++ "px,-" ++ (toString <| sizepx / 2) ++ "px)")
                        , Color.text Color.white
                        ]
                    ]
    in
        Options.div
            [ css "min-height" "300px"
            , css "background-color" "black"
            , css "position" "relative"
            ]
            [ Html.img
                [ Attrs.id "restaurant-img"
                , Attrs.attribute "aria-live" "polite"
                , Attrs.attribute "aria-atomic" "true"
                , Attrs.attribute "aria-relevant" "text"
                , Attrs.src src
                , Attrs.alt <| "Image " ++ toString (Zipper.index photos) ++ ": " ++ alt
                , Attrs.style
                    [ ( "position", "absolute" )
                    , ( "top", "0" )
                    , ( "bottom", "0" )
                    , ( "left", "0" )
                    , ( "right", "0" )
                    , ( "height", "100%" )
                    , ( "margin", "auto" )
                    , ( "z-index", "0" )
                    ]
                ]
                []
            , Options.div
                [ css "width" "100%"
                , css "height" "100px"
                , Options.scrim 0.75
                , css "position" "absolute"
                , css "bottom" "0"
                , Color.text Color.white
                , css "z-index" "2"
                ]
                [ Html.h2 [ invisible ] [ Html.text <| "Details on " ++ r.name ]
                , title name
                , subhead address
                , subhead openingHoursToday
                ]
            , Html.fieldset
                [ Attrs.attribute "aria-controls" "restaurant-img"
                ]
                [ prevPhotoBtn
                , nextPhotoBtn
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
            [ size Desktop 6, size Tablet 4, size Phone 4 ]
            [ newReview r m ]
        , cell
            [ size Desktop 6
            , size Tablet 4
            , size Phone 4
            , css "padding" "5% 0"
            ]
            [ Html.h2 [ invisible ] [ Html.text "Overall rating" ]
            , sexyRating r m
            ]
        ]


{-|
-}
newReview : Restaurant -> Model -> Html Msg
newReview r m =
    let
        { newReview } =
            m
    in
        grid
            [ css "min-height" "300px"
            ]
            [ cell [ size All 12 ]
                [ Html.h2 [ invisible ] [ Html.text "New Review" ]
                , Textfield.render
                    Mdl
                    [ 1 ]
                    m.mdl
                    [ Textfield.label "Name "
                    , Textfield.text'
                    , Textfield.value newReview.authorName
                    , Textfield.onInput <| OnUpdateNewReview << UpdateName
                    , css "width" "100%"
                    , Options.inner
                        [ Options.attribute <| Attrs.attribute "aria-labelledby" "nameinput-label"
                        , Options.attribute <| Attrs.name "your name"
                        ]
                    ]
                , Html.span
                    [ Attrs.id "nameinput-label", Attrs.hidden True ]
                    [ Html.text "Your name" ]
                ]
            , cell [ size All 12 ]
                [ Textfield.render Mdl
                    [ 0 ]
                    m.mdl
                    [ Textfield.label "Comments"
                    , Textfield.textarea
                    , Textfield.rows 3
                    , Textfield.value newReview.text
                    , Textfield.onInput <| OnUpdateNewReview << UpdateText
                    , css "width" "100%"
                    , Options.inner
                        [ css "resize" "none"
                        , Options.attribute <| Attrs.attribute "aria-labelledby" "commentsinput-label"
                        , Options.attribute <| Attrs.name "comments"
                        , Options.attribute <| Attrs.attribute "aria-multiline" "true"
                        ]
                    ]
                , Html.span
                    [ Attrs.id "commentsinput-label", Attrs.hidden True ]
                    [ Html.text "Comments" ]
                ]
            , cell [ size All 12 ] [ stars 48 newReview.rating m ]
            , cell [ size All 12 ]
                [ Button.render Mdl
                    [ 2 ]
                    m.mdl
                    [ Button.onClick OnNewReviewSubmitBtnPressed
                    , Button.raised
                    , Button.colored
                    , css "width" "100%"
                    , css "margin-top" "8px"
                    , css "text-transform" "none"
                    ]
                    [ text "Add Review" ]
                ]
            ]


stars : Int -> Rating -> Model -> Html Msg
stars sizepx r m =
    let
        tabindex i =
            if i == One then
                [ Attrs.tabindex 0 ]
            else
                []

        color i =
            if i `Helper.lte` r then
                Color.primary
            else
                Color.color Color.Grey Color.S400

        starOrstars i =
            if i == One then
                " star"
            else
                " stars"

        star i =
            Html.label
                [ Attrs.style
                    [ ( "width", "20%" )
                    , ( "display", "inline-block" )
                    ]
                ]
                [ Html.input
                    ([ Attrs.type' "radio"
                     , Attrs.name "rating"
                     , Attrs.value <| ratingToString i
                     , Attrs.title (ratingToString i ++ (starOrstars i))
                     , Attrs.style [ ( "left", "-999999px" ), ( "position", "absolute" ) ]
                     , Events.onCheck
                        (\checked ->
                            if checked then
                                OnUpdateNewReview (UpdateRating i)
                            else
                                NoOp
                        )
                     , Attrs.checked (r == i)
                     ]
                        ++ (tabindex i)
                    )
                    []
                , Icon.view
                    "star"
                    [ Color.text (color i)
                    , Typography.contrast 1.0
                    , css "font-size" <| toString sizepx ++ "px"
                    , css "cursor" "pointer"
                    ]
                ]
    in
        Options.div
            [ css "text-align" "center" ]
            (List.map star [ One, Two, Three, Four, Five ])


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
            , Options.attribute <| Attrs.attribute "role" "textbox"
            , Options.attribute <| Attrs.attribute "aria-readonly" "true"
            ]
            [ Html.text <| (toStringf avgRating)
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

        { newReviews } =
            m

        newReviewToReview newReview =
            { authorName = newReview.authorName
            , time = Maybe.withDefault Time.epoch newReview.time
            , rating = newReview.rating
            , text = newReview.text
            }

        userReviews =
            case Dict.get id newReviews of
                Just reviews ->
                    List.map newReviewToReview reviews

                Nothing ->
                    []

        allReviews =
            userReviews ++ reviews
    in
        List.ul
            []
            (List.map (flip userReview m) allReviews)


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
                , Options.span
                    [ Typography.caption
                    , css "color" "rgb(118,118,118)"
                    , css "opacity" "1"
                    ]
                    [ Html.text <| " on " ++ formatedLocalTime ]
                , List.body
                    [ css "min-height" "52px"
                    , css "height" "auto"
                    ]
                    [ Options.span
                        [ css "font-weight" "600"
                        , css "margin-right" "8px"
                        , css "color" "rgb(89,89,89)"
                        , Options.attribute <| Attrs.attribute "role" "textbox"
                        , Options.attribute <| Attrs.attribute "aria-readonly" "true"
                        ]
                        [ Html.text <| (ratingToString rating)
                        , Icon.view
                            "star"
                            [ Color.text Color.primary
                            , css "position" "relative"
                            , css "top" "5px"
                            ]
                        ]
                    , Options.span
                        [ css "color" "rgb(89,89,89)"
                        ]
                        [ Html.text text ]
                    ]
                ]
            ]

module Views.Pages.Home exposing (view)

import Types exposing (..)
import Model exposing (..)
import Msg exposing (..)
import Components.Autocomplete as Autocomplete
import Helper exposing (prices, priceString, cuisines, cuisineString)
import Html exposing (Html, map, div, text, img, h4, select, option)
import Html.Attributes as Attr
import Html.Events as Events
import Material.Grid exposing (grid, cell, size, Device(..), noSpacing, Cell, offset)
import Material.List exposing (ul, li)
import Material.Card as Card
import Material.Toggles as Toggles exposing (switch, value)
import Material.Button as Button exposing (render, colored, primary)
import Material.Elevation as Elevation
import Material.Options as Options exposing (css)
import Material.Color as Color


cuisineAutocompleteViewConfig : Autocomplete.ViewConfig Cuisine
cuisineAutocompleteViewConfig =
    let
        customLi keySelected mouseSelected i cuisine =
            { attributes =
                [ Attr.id (cuisineString cuisine)
                , Attr.classList
                    [ ( "mdl-list__item", True )
                    , ( "active", keySelected || mouseSelected )
                    ]
                , Attr.style [ ( "background-color", "white" ) ]
                , Attr.attribute "role" "option"
                ]
            , children = [ Html.text (cuisineString cuisine) ]
            }
    in
        { toId = cuisineString
        , ul =
            [ Attr.class "mdl-list"
            , Attr.style
                [ ( "position", "absolute" )
                , ( "width", "100%" )
                , ( "z-index", "2" )
                , ( "margin-top", "-19px" )
                ]
            ]
        , li = customLi
        , inputLabel = "Select cuisine type"
        }


view : Model -> Html Msg
view model =
    let
        { restaurants, loaderDisplayed, errMsg, mdl, cuisineAutocomplete, includeCasualInSearch, includeFancyInSearch, openNow } =
            model
    in
        (Options.styled_ div)
            []
            [ Attr.style [ ( "min-height", "500px" ), ( "padding-top", "2%" ) ] ]
            [ grid
                []
                [ cell
                    [ size Desktop 4, size Tablet 4, size Phone 4, offset Desktop 4, offset Tablet 2 ]
                    [ map CuisineAutocomplete (Autocomplete.view cuisineAutocompleteViewConfig cuisineAutocomplete cuisines)
                    ]
                , cell
                    [ size Desktop 2, size Tablet 2, size Phone 2, offset Desktop 4, offset Tablet 2 ]
                    [ Toggles.checkbox Mdl
                        [ 0 ]
                        model.mdl
                        [ Toggles.value includeCasualInSearch
                        , Toggles.ripple
                        , Toggles.onClick ToggleCasual
                        , Options.inner
                            [ Options.attribute <| Attr.attribute "aria-label" "Include casual restaurants"
                            ]
                        ]
                        [ text "Casual" ]
                    , Toggles.checkbox Mdl
                        [ 1 ]
                        model.mdl
                        [ Toggles.value includeFancyInSearch
                        , Toggles.ripple
                        , Toggles.onClick ToggleFancy
                        , Options.inner
                            [ Options.attribute <| Attr.attribute "aria-label" "Include fancy restaurants"
                            ]
                        ]
                        [ text "Fancy" ]
                    , Toggles.checkbox Mdl
                        [ 2 ]
                        mdl
                        [ Toggles.onClick ToggleOpenNow
                        , value openNow
                        , Options.inner
                            [ Options.attribute <| Attr.attribute "aria-label" "Include only open restaurants." ]
                        ]
                        [ text "Open now" ]
                    ]
                , cell
                    [ size Desktop 2, size Tablet 2, size Phone 2 ]
                    [ div
                        []
                        [ Button.render Mdl
                            [ 1 ]
                            mdl
                            [ Button.onClick FetchRestaurants
                            , Button.raised
                            , Button.colored
                            , css "width" "100%"
                            ]
                            [ text "Search" ]
                        ]
                    ]
                , cell
                    [ size Desktop 10
                    , offset Desktop 1
                    , size Tablet 6
                    , offset Tablet 1
                    , size Phone 4
                    ]
                    [ Options.div
                        [ css "display" "flex"
                        , css "flex-flow" "row wrap"
                        , css "justify-content" "center"
                        , css "width" "100%"
                        , Options.attribute <| Attr.attribute "role" "list"
                        , Elevation.e2
                        ]
                        (List.indexedMap (restaurantCard model) restaurants)
                    ]
                ]
            ]


restaurantCard : Model -> Int -> RestaurantPreview -> Html Msg
restaurantCard { indexOfElevatedCard } i r =
    let
        { name, types, address } =
            r

        elevation =
            case indexOfElevatedCard of
                Nothing ->
                    Elevation.e0

                Just k ->
                    if i == k then
                        Elevation.e6
                    else
                        Elevation.e0

        zIndex =
            case indexOfElevatedCard of
                Nothing ->
                    "0"

                Just k ->
                    if i == k then
                        "3"
                    else
                        "0"
    in
        Card.view
            [ css "height" "138px"
            , css "width" "50%"
            , css "z-index" zIndex
            , Color.background Color.primary
            , Elevation.transition 250
            , elevation
            , Options.attribute <| Events.onMouseEnter <| MouseEnterRestaurantCard (Just i)
            , Options.attribute <| Events.onMouseLeave <| MouseEnterRestaurantCard Nothing
            , Options.attribute <| Events.onClick <| OnRestaurantClick r
            ]
            [ Card.title
                [ Options.attribute <| Attr.attribute "role" "listitem"
                ]
                [ Card.head
                    [ Color.text Color.white
                    , Options.attribute <| Attr.attribute "role" "link"
                    ]
                    [ text name ]
                , Card.subhead [ Color.text Color.white ] [ text address ]
                ]
            ]

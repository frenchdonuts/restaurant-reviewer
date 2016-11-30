module Views.Pages.Home exposing (view)

import Types exposing (..)
import Model exposing (..)
import Msg exposing (..)
import Components.Autocomplete as Autocomplete
import Helper exposing (prices, priceString, cuisines, cuisineString)
import Html exposing (Html, div, text, img, h4, select, option)
import Html.Attributes as Attr
import Html.Events as Events
import Html.App exposing (map)
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
        customLi keySelected mouseSelected cuisine =
            { attributes =
                [ Attr.classList
                    [ ( "mdl-list__item", True )
                    , ( "active", keySelected )
                    , ( "active", mouseSelected )
                    ]
                , Attr.style [ ( "background-color", "white" ) ]
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
        , inputLabel = "Cuisine"
        }


view : Model -> Html Msg
view model =
    let
        { restaurants, loaderDisplayed, errMsg, mdl, cuisineAutocomplete, includeCasualInSearch, includeFancyInSearch, openNow } =
            model
    in
        (Options.styled' div)
            []
            [ Attr.style [ ( "min-height", "500px" ), ( "padding-top", "2%" ) ] ]
            [ grid
                []
                [ cell
                    [ size Desktop 4, size Tablet 4, size Phone 4, offset Desktop 4, offset Tablet 2 ]
                    [ Autocomplete.view cuisineAutocompleteViewConfig cuisineAutocomplete cuisines |> map CuisineAutocomplete
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
                            , Options.attribute <| Attr.attribute "aria-checked" (toString ToggleCasual)
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
                            , Options.attribute <| Attr.attribute "aria-checked" (toString ToggleFancy)
                            ]
                        ]
                        [ text "Fancy" ]
                    ]
                , cell
                    [ size Desktop 2, size Tablet 2, size Phone 2, offset Tablet 2 ]
                    [ switch Mdl
                        [ 2 ]
                        mdl
                        [ Toggles.onClick ToggleOpenNow
                        , value openNow
                        , Options.inner
                            [ Options.attribute <| Attr.attribute "aria-label" "Open now"
                            , Options.attribute <| Attr.attribute "aria-checked" (toString ToggleOpenNow)
                            ]
                        ]
                        [ text "Open now" ]
                    , div
                        [ Attr.attribute "role" "button"
                        , Attr.style [ ( "float", "right" ), ( "padding-right", "16px" ) ]
                        ]
                        [ Button.render Mdl
                            [ 1 ]
                            mdl
                            [ Button.primary
                            , Button.onClick FetchRestaurants
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
              --, css "border" "1px solid white"
            , css "z-index" zIndex
            , Color.background Color.primary
            , Elevation.transition 250
            , elevation
            , Options.attribute <| Events.onMouseEnter <| MouseEnterRestaurantCard (Just i)
            , Options.attribute <| Events.onMouseLeave <| MouseEnterRestaurantCard Nothing
            , Options.attribute <| Events.onClick <| OnRestaurantClick r
            ]
            [ Card.title
                [ Options.attribute <| Attr.tabindex 0
                , Options.attribute <| Attr.attribute "role" "listitem"
                ]
                [ Card.head [ Color.text Color.white ] [ text name ]
                , Card.subhead [ Color.text Color.white ] [ text address ]
                ]
            ]

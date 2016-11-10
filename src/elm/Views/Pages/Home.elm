module Views.Pages.Home exposing (view)

import Types exposing (..)
import Model exposing (..)
import Msg exposing (..)
import Components.Autocomplete as Autocomplete
import Helper exposing (prices, priceString, cuisines, cuisineString)
import Html exposing (Html, div, text, img, h4, select, option)
import Html.Attributes as Attr
import Html.App exposing (map)
import Material.Grid exposing (grid, cell, size, Device(..), noSpacing, Cell, offset)
import Material.List exposing (ul, li)
import Material.Toggles as Toggles exposing (switch, value)
import Material.Button as Button exposing (render, colored, primary)


--import Material.Card as Card exposing (jj)


cuisineAutocompleteViewConfig : Autocomplete.ViewConfig Cuisine
cuisineAutocompleteViewConfig =
    let
        -- TODO: Give appropriate attributes for when keySelected or mouseSelected
        customLi keySelected mouseSelected cuisine =
            { attributes =
                [ Attr.classList
                    [ ( "collection-item", True )
                    , ( "active", keySelected )
                    , ( "active", mouseSelected )
                    ]
                ]
            , children = [ Html.text (cuisineString cuisine) ]
            }
    in
        { toId = cuisineString
        , ul =
            [ Attr.class "collection"
            , Attr.style [ ( "position", "absolute" ), ( "width", "100%" ), ( "z-index", "2" ), ( "margin-top", "-19px" ) ]
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
        div
            []
            [ grid
                []
                ([ cell
                    [ size All 3 ]
                    [ Autocomplete.view cuisineAutocompleteViewConfig cuisineAutocomplete cuisines
                        |> map CuisineAutocomplete
                    ]
                 , cell
                    [ size All 3 ]
                    [ div
                        []
                        [ Toggles.checkbox Mdl
                            [ 0 ]
                            model.mdl
                            [ Toggles.value includeCasualInSearch
                            , Toggles.ripple
                            , Toggles.onClick ToggleCasual
                            ]
                            [ text "Casual" ]
                        , Toggles.checkbox Mdl
                            [ 1 ]
                            model.mdl
                            [ Toggles.value includeFancyInSearch
                            , Toggles.ripple
                            , Toggles.onClick ToggleFancy
                            ]
                            [ text "Fancy" ]
                        ]
                    ]
                 , cell
                    [ size All 3 ]
                    [ switch Mdl [ 2 ] mdl [ Toggles.onClick ToggleOpenNow, value openNow ] [ text "Open now" ] ]
                 , cell
                    [ size All 3 ]
                    [ div
                        [ Attr.attribute "role" "button" ]
                        [ Button.render Mdl
                            [ 1 ]
                            mdl
                            [ Button.onClick FetchRestaurants, primary ]
                            [ text "Search" ]
                        ]
                    ]
                 ]
                    ++ (List.map restaurantCard restaurants)
                )
            ]


restaurantCard : Restaurant -> Cell a
restaurantCard r =
    cell
        [ size All 12 ]
        [ li
            []
            [ text r.name
            , img [] [ text r.photoUrl ]
            ]
        ]

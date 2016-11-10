module Views.Pages.Home exposing (view)

import Types exposing (..)
import Model exposing (..)
import Msg exposing (..)
import Components.Autocomplete as Autocomplete
import Components.Dropdown.View as Dropdown
import Helper exposing (prices, priceString, cuisines, cuisineString)
import Html exposing (Html, div, text, img, h4, select, option)
import Html.Attributes as Attr
import Html.App exposing (map)
import Material.Grid exposing (grid, cell, size, Device(..), noSpacing, Cell, offset)
import Material.List exposing (ul, li)
import Material.Toggles as Toggles exposing (switch, value)
import Material.Button as Button exposing (render, colored, primary)


--import Material.Card as Card exposing (jj)

import Material.Options exposing (attribute)


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
        { restaurants, restaurantFilters, loaderDisplayed, errMsg, mdl, cuisineAutocomplete, priceDropdown } =
            model

        { cuisine, openNow, maxPrice } =
            restaurantFilters
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
                    [ Dropdown.root
                        (List.map priceString prices)
                        priceDropdown
                        |> map PriceDropdown
                    ]
                 , cell
                    [ size All 6 ]
                    [ switch Mdl [ 0 ] mdl [ value openNow ] [ text "Open now" ] ]
                 , cell
                    [ size All 6 ]
                    [ div
                        [ Attr.attribute "role" "button" ]
                        [ Button.render Mdl
                            [ 1 ]
                            mdl
                            [ primary ]
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

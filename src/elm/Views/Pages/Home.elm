module Views.Pages.Home exposing (view)

import Types exposing (..)
import Model exposing (..)
import Msg exposing (..)
import Html exposing (Html, div, ul, text, img)


view : Model -> Html Msg
view { restaurants, restaurantFilters, loaderDisplayed, errMsg } =
    div
        []
        [ ul [] (List.map restaurantCard restaurants)
        ]


restaurantCard : Restaurant -> Html Msg
restaurantCard r =
    div
        []
        [ text r.name
        , img [] [ text r.photoUrl ]
        ]

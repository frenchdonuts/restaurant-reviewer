module Pages.Home.View exposing (..)

import Pages.Home.Types exposing (..)
import Html exposing (..)


root : Model -> Html Msg
root { restaurants, restaurantFilters, loaderDisplayed, errMsg } =
    div [] (List.map restaurantCard restaurants)


restaurantCard : Restaurant -> Html Msg
restaurantCard r =
    div
        []
        [ text r.name
        , img [] [ text r.photoUrl ]
        ]

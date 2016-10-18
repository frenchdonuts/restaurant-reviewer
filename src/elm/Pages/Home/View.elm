module Pages.Home.View exposing (..)

import Pages.Home.Types exposing (..)
import Html exposing (..)


root : Model -> Html Msg
root (Model { restaurants, restaurantFilters }) =
    div [] (List.map restaurantCard restaurants)


restaurantCard : Restaurant -> Html Msg
restaurantCard r =
    div [] []

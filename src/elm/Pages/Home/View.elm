module Pages.Home.View exposing (..)

import Pages.Home.Types exposing (..)
import Html exposing (..)


{-| How will the UI look?
    Card
        Toggle in upper right corner
        2 Selectors in 1 row - Cuisine and (max)Price
-}
root : Model -> Html Msg
root { restaurants, restaurantFilters, loaderDisplayed, errMsg } =
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
